function(input, output, session){
  
  ## Data filtering ----------------
    filtered_station <- eventReactive(input$submit,{
    ec_data |> 
      filter(location_id == input$station) |> 
      arrange(datetime)
  }, ignoreNULL = FALSE)
    
    filtered_df <- reactive({
    filtered_station() |> 
      filter(datetime >= input$start_date,
             datetime <= input$end_date) |> 
      arrange(datetime)
  })
  # xts_orig <- reactive({
  #   df <- filtered_df()
  #   df$datetime <- as.POSIXct(df$datetime, tz = "America/Los_Angeles")
  #   xts(df["parameter_value"], order.by = df$datetime)
  # })
  # 
  
  ## Modify select input ----------------
  observeEvent(input$station, {
    station_vals <- ec_data |> 
      filter(location_id == input$station) |> 
      pull(parameter_value)
    
    new_min <- unname(pmax(min(station_vals, na.rm = TRUE) * 0.9, 1))
    new_max <- unname(ceiling(quantile(station_vals, 0.999, na.rm = TRUE) * 1.5))
    
    step_vals <- abs(diff(station_vals))
    new_j <- unname(ceiling(quantile(step_vals, 0.999, na.rm = TRUE)) * 1.25)
    
    updateSliderInput(session, "physical_limits", value = c(new_min, new_max))
    updateSliderInput(session, "j", value = new_j)
  }, ignoreNULL = FALSE)
  
  ## Display station name -------------
  station_display <- reactive({
    df <- filtered_df()
    location = unique(df$location_id)
    x <- stations |> filter(cdec_id == location) |> select(cdec_id, description)
    paste0(x$cdec_id, ": ", x$description)
  })
  
  output$station_text <- renderText({
   station_display()
  })
  
  ## Apply flagger to filtered data -------------
  flagged_df <- eventReactive(input$submit,{
    df <- filtered_df()
    flags <- flagger(x = df$parameter_value,
                     warmup = input$warmup,
                            j = input$j,
                            k = input$k,
                            consec_threshold = input$consec_thr,
                            stuck_threshold = input$stuck_thr,
                            physical_min = input$physical_limits[1],
                            physical_max = input$physical_limits[2]
                     )
   df |> 
      mutate(flag = flags[["flag"]],
             flag_ann = flags[["annotation"]]
      )
  },ignoreNULL = FALSE)
  
### xts format data -----------------
    xts_flagged <- reactive({
    df <- flagged_df()
    df$datetime <- as.POSIXct(df$datetime, tz = "America/Los_Angeles")
    
    ann_lvls <- sort(unique(na.omit(df$flag_ann)))
    
    df <- df |>
      mutate(good = if_else(!flag, parameter_value, NA_real_),
             warmup = if_else(flag_ann=="warm-up", parameter_value, NA_real_),
             warmup_extreme = if_else(flag_ann=="warm-up extreme value", parameter_value, NA_real_),
             extreme = if_else(flag_ann=="extreme value", parameter_value, NA_real_),
             stuck = if_else(flag_ann=="stuck value", parameter_value, NA_real_),
             stat_outlier = if_else(flag_ann=="hampel & jump", parameter_value, NA_real_),
             flag_ann_code = as.integer(factor(flag_ann, levels = ann_lvls)))
    
    x <- xts(df[c("good", "warmup", "warmup_extreme", "extreme", "stuck", "stat_outlier", "flag_ann_code")], order.by = df$datetime)
    attr(x, "ann_levels") <- ann_lvls
    x
  })
  
  ### highlight NAs ------------
  na_gaps <- reactive({
    df <- flagged_df()
    is_na <- is.na(df$parameter_value)
    r <- rle(is_na)
    ends <- cumsum(r$lengths)
    starts <- ends - r$lengths + 1
    gap_rows <- which(r$values)
    if (length(gap_rows) == 0) return(NULL)
    data.frame(
      start = df$datetime[starts[gap_rows]],
      end   = df$datetime[ends[gap_rows]]
    )
  })
  
  ## Tab 1 -------------------
  ### Plots -------------
  
  #### Plot 1 -------------------
  output$original_plot <- renderDygraph({
    df <- xts_flagged()
    ann_lvls <- attr(df, "ann_levels")
    
    dg <- dygraph(df, main = "Full time range of EC data") |> 
      dySeries("good", label = "Good",
               color = "rgba(153, 153, 153, 0.3)", 
               pointSize = 1, strokeWidth = 1) |>
      dySeries("warmup", label = "warmup",
               color = "rgba(232, 109, 176, 0.9)",
               pointSize = 4, strokeWidth = 2) |>
      dySeries("warmup_extreme", label = "warmup & extreme",
               color = "rgba(168, 15, 103, 0.9)", 
               pointSize = 4, strokeWidth = 2) |>
      dySeries("extreme", label = "extreme",
               color = "rgba(230, 159, 0, 0.9)", 
               pointSize = 4, strokeWidth = 2) |>
      dySeries("stuck", label = "stuck value",
               color = "rgba(0, 114, 178, 0.9)", 
               pointSize = 4, strokeWidth = 2) |>
      dySeries("stat_outlier", label = "outlier/spike",
               color = "rgba(0, 158, 115, 0.9)", 
               pointSize = 4, strokeWidth = 2) |>
      # include but hide annotations
      dySeries("flag_ann_code", axis = "y2", 
               strokeWidth = 0, drawPoints = FALSE, color = "rgba(0,0,0,0)") |>
      dyAxis("y2", drawGrid = FALSE, valueRange = c(0, 1), independentTicks = FALSE) |>
      # normal y axis
      dyAxis("y", label = "EC (uS/cm)", valueRange = c(-30, max(df$parameter_value, na.rm = TRUE) * 1.05)) |>
      # x axis padding
      dyAxis("x", rangePad = 10) |> 
      dyOptions(
        connectSeparatedPoints = FALSE,
        useDataTimezone = TRUE,
        drawGrid = TRUE,
        drawPoints = TRUE,
        pointSize = 3) |> 
      # custom legend
      dyCallbacks(
        drawHighlightPointCallback = JS(
          "function(g, seriesName, canvasContext, cx, cy, color, pointSize) {
            if(seriesName === 'flag_ann_code') return;
            canvasContext.beginPath();
            canvasContext.fillStyle = '#333333';
            canvasContext.strokeStyle = 'white';
            canvasContext.lineWidth = 1;
            canvasContext.arc(cx, cy, pointSize * 1.5, 0, 2 * Math.PI, false);
            canvasContext.fill();
            canvasContext.stroke();
          }"
        ),
        zoomCallback = JS("
          function(minX, maxX, yRanges) {
            if (window.__ecSync) return;
            window.__ecSync = true;
            try {
              var targetEl = HTMLWidgets.find('#qc_plot');
              var target = targetEl ? targetEl.dygraph : null;
              if (target) target.updateOptions({dateWindow: [minX, maxX]});
        
              var selfEl = HTMLWidgets.find('#original_plot');
              var self = selfEl ? selfEl.dygraph : null;
              if (self) self.updateOptions({dateWindow: null});
            } finally {
              window.__ecSync = false;   // always clears, even if something above throws
            }
          }
        "),
        # highlight color based on selection from qc_plot
        underlayCallback = JS("
          function(canvas, area, g) {
            var detailEl = HTMLWidgets.find('#qc_plot');
            var detail = detailEl ? detailEl.dygraph : null;
            if (!detail) return;        
            var dw = detail.xAxisRange();
            var x0 = g.toDomXCoord(dw[0]);
            var x1 = g.toDomXCoord(dw[1]);
            canvas.fillStyle = 'rgba(255, 221, 0, 0.25)'; //highlight color specified here
            canvas.fillRect(x0, area.y, x1 - x0, area.h);
            canvas.strokeStyle = 'rgba(230, 126, 0, 0.9)';
            canvas.lineWidth = 1.5;
            canvas.strokeRect(x0, area.y, x1 - x0, area.h);
          }
        "),
        highlightCallback = JS(sprintf("
        function(event, x, points, row, seriesName) {
          var annLevels = %s;
          var ecPoint = points.find(function(p){ return p.name !== 'flag_ann_code' && !isNaN(p.yval); });
          var annPoint = points.find(function(p){ return p.name === 'flag_ann_code'; });
          var ecTxt = ecPoint ? ecPoint.yval.toFixed(1) : 'NA';
          var annCode = annPoint ? annPoint.yval : null;
          var annTxt = (annCode != null && annLevels[annCode - 1] != null) ? annLevels[annCode - 1] : 'Good';
          var tooltip = document.getElementById('qc_tooltip');
          tooltip.innerHTML = '<strong>' + new Date(x).toLocaleString() + '</strong><br>EC: ' + ecTxt + ' uS/cm<br>Flag: ' + annTxt;
          tooltip.style.left = (event.clientX + 15) + 'px';
          tooltip.style.top = (event.clientY + 15) + 'px';
          tooltip.style.display = 'block';
        }
", jsonlite::toJSON(ann_lvls))),
        unhighlightCallback = JS(
          "function(event) {
            document.getElementById('qc_tooltip').style.display = 'none';
          }"
        )
      ) |> 
      dyHighlight(
        highlightCircleSize = 3,
        highlightSeriesBackgroundAlpha = 0.5,
        hideOnMouseOut = TRUE
      ) |> 
      dyLegend(show = "never") |> 
      dyCrosshair(direction = "vertical") |> 
      dyUnzoom()
    
    dg$x$attrs$axes$y2$drawAxis <- FALSE
    
    # highlight gaps
    gaps <- na_gaps()
    if (!is.null(gaps)) {
      for (i in seq_len(nrow(gaps))) {
        dg <- dyShading(dg, from = gaps$start[i], to = gaps$end[i], color = "#BEE6E9")
      }
    }
    
    dg
  })
    

  #    dg <- dygraph(df, main = "Unflagged EC Data") |> 
  #     dySeries("parameter_value", label = "EC (uS/cm)", axis = "y") |> 
  #     dyAxis("y", label = "EC (uS/cm)", valueRange = c(-30, max(df$parameter_value, na.rm = TRUE) * 1.05)) |>
  #     dyAxis("x", rangePad = 10) |> 
  #     dyOptions(
  #       connectSeparatedPoints = TRUE,
  #       useDataTimezone = TRUE,
  #       drawGrid = TRUE,
  #       drawPoints = TRUE,
  #       pointSize = 2) |> 
  #     dyCallbacks(
  #       drawHighlightPointCallback = JS(
  #         "function(g, seriesName, canvasContext, cx, cy, color, pointSize) {
  #        canvasContext.beginPath();
  #        canvasContext.fillStyle = '#333333';      
  #        canvasContext.strokeStyle = 'white';  
  #        canvasContext.lineWidth = 1;
  #        canvasContext.arc(cx, cy, pointSize * 1.5, 0, 2 * Math.PI, false);
  #        canvasContext.fill();
  #        canvasContext.stroke();
  #      }"
  #       ),
  #       zoomCallback = JS("
  #       function(minX, maxX, yRanges) {
  #         var target = HTMLWidgets.find('#qc_plot').dygraph;
  #         if (target) target.updateOptions({dateWindow: [minX, maxX]});
  #         var self = HTMLWidgets.find('#original_plot').dygraph;
  #         if (self) self.updateOptions({dateWindow: null});
  #       }
  #     ")
  #     ) |> 
  #     dyHighlight(
  #       highlightSeriesBackgroundAlpha = 0.5,
  #       hideOnMouseOut = TRUE
  #     ) |> 
  #     dyLegend(show = "onmouseover", hideOnMouseOut = TRUE) |> 
  #     dyCrosshair(direction = "vertical") |> 
  #     dyUnzoom()
  #   
  #   dg
  # })
  # 
  # 
  
  output$qc_plot <- renderDygraph({
    df <- xts_flagged()
    ann_lvls <- attr(df, "ann_levels")
    
    dg <- dygraph(df, main = "Zoomed in EC Data") |> 
      dySeries("good", label = "Good",
               color = "rgba(153, 153, 153, 0.3)", 
               pointSize = 1, strokeWidth = 1) |>
      dySeries("warmup", label = "warmup",
               color = "rgba(232, 109, 176, 0.9)",
               pointSize = 4, strokeWidth = 2) |>
      dySeries("warmup_extreme", label = "warmup & extreme",
               color = "rgba(168, 15, 103, 0.9)", 
               pointSize = 4, strokeWidth = 2) |>
      dySeries("extreme", label = "extreme",
               color = "rgba(230, 159, 0, 0.9)", 
               pointSize = 4, strokeWidth = 2) |>
      dySeries("stuck", label = "stuck value",
               color = "rgba(0, 114, 178, 0.9)", 
               pointSize = 4, strokeWidth = 2) |>
      dySeries("stat_outlier", label = "outlier/spike",
               color = "rgba(0, 158, 115, 0.9)", 
               pointSize = 4, strokeWidth = 2) |>
      # include but hide annotations
      dySeries("flag_ann_code", axis = "y2", 
               strokeWidth = 0, drawPoints = FALSE, color = "rgba(0,0,0,0)") |>
      dyAxis("y2", drawGrid = FALSE, valueRange = c(0, 1), independentTicks = FALSE) |>
      # normal y axis
      dyAxis("y", label = "EC (uS/cm)", valueRange = c(-30, max(df$parameter_value, na.rm = TRUE) * 1.05)) |>
      # x axis padding
      dyAxis("x", rangePad = 10) |> 
      dyOptions(
        connectSeparatedPoints = FALSE,
        useDataTimezone = TRUE,
        drawGrid = TRUE,
        drawPoints = TRUE,
        pointSize = 3) |> 
    dyRangeSelector() |> 
    dyCallbacks(
      clickCallback = JS(
      "function(e, x, points) {
           Shiny.setInputValue('qc_click', {x: x}, {priority: 'event'});
         }"
      ),
      zoomCallback= JS(
      "function(minDate, maxDate, yRanges) {
           Shiny.setInputValue('qc_zoom', {min: minDate, max: maxDate}, {priority: 'event'});
         }"
      ),
      drawCallback = JS("
        function(g, isInitial) {
          if (isInitial || window.__ecSync) return;
          window.__ecSync = true;
          try {
            var overviewEl = HTMLWidgets.find('#original_plot');
            var overview = overviewEl ? overviewEl.dygraph : null;
            if (overview) overview.updateOptions({});
          } finally {
            window.__ecSync = false;   // always clears, even if resize() throws
          }
        }
      "),
      # custom legend
      drawHighlightPointCallback = JS(
        "function(g, seriesName, canvasContext, cx, cy, color, pointSize) {
            if(seriesName === 'flag_ann_code') return;
            canvasContext.beginPath();
            canvasContext.fillStyle = '#333333';
            canvasContext.strokeStyle = 'white';
            canvasContext.lineWidth = 1;
            canvasContext.arc(cx, cy, pointSize * 1.5, 0, 2 * Math.PI, false);
            canvasContext.fill();
            canvasContext.stroke();
          }"
      ),
      highlightCallback = JS(sprintf("
  function(event, x, points, row, seriesName) {
    var annLevels = %s;
    var ecPoint = points.find(function(p){ return p.name !== 'flag_ann_code' && !isNaN(p.yval); });
    var annPoint = points.find(function(p){ return p.name === 'flag_ann_code'; });
    var ecTxt = ecPoint ? ecPoint.yval.toFixed(1) : 'NA';
    var annCode = annPoint ? annPoint.yval : null;
    var annTxt = (annCode != null && annLevels[annCode - 1] != null) ? annLevels[annCode - 1] : 'Good';
    var tooltip = document.getElementById('qc_tooltip');
    tooltip.innerHTML = '<strong>' + new Date(x).toLocaleString() + '</strong><br>EC: ' + ecTxt + ' uS/cm<br>Flag: ' + annTxt;
    tooltip.style.left = (event.clientX + 15) + 'px';
    tooltip.style.top = (event.clientY + 15) + 'px';
    tooltip.style.display = 'block';
  }
", jsonlite::toJSON(ann_lvls))),
      unhighlightCallback = JS(
        "function(event) {
            document.getElementById('qc_tooltip').style.display = 'none';
          }"
      )
    ) |> 
      dyHighlight(
        highlightCircleSize = 3,
        highlightSeriesBackgroundAlpha = 0.5,
        hideOnMouseOut = TRUE
      ) |> 
      dyLegend(show = "never") |> 
      dyCrosshair(direction = "vertical") |> 
      dyUnzoom()
    
    dg$x$attrs$axes$y2$drawAxis <- FALSE
    
    # highlight gaps
    gaps <- na_gaps()
    if (!is.null(gaps)) {
      for (i in seq_len(nrow(gaps))) {
        dg <- dyShading(dg, from = gaps$start[i], to = gaps$end[i], color = "#BEE6E9")
      }
    }
    
    dg
  })
  
  
  ### Saving annotations ----------------
  pending_range <- reactiveVal(NULL)
  pending_kind  <- reactiveVal(NULL)
  
  ms_to_time <- function(ms) as.POSIXct(ms / 1000, origin = "1970-01-01", tz = "UTC")
  
  observeEvent(input$qc_click, {
    req(isTRUE(input$annotate_mode))  
    t <- ms_to_time(input$qc_click$x)
    pending_range(c(t, t))
    pending_kind("point")
    removeNotification("annotate_prompt")
    updateCheckboxInput(session, "annotate_mode", value = FALSE)  # disarm
    open_note_modal()
  })
  
  observeEvent(input$qc_zoom, {
    req(isTRUE(input$annotate_mode)) 
    pending_range(c(ms_to_time(input$qc_zoom$min), ms_to_time(input$qc_zoom$max)))
    pending_kind("range")
    removeNotification("annotate_prompt")
    updateCheckboxInput(session, "annotate_mode", value = FALSE)
    open_note_modal()
  })
  
  open_note_modal <- function() {
    showModal(modalDialog(
      title = "Add QC note",
      textInput("flag_type", "Reason / flag type"),
      textAreaInput("note_detail", "Notes"),
      textInput("person_logging", "Logged by"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_note", "Save")
      )
    ))
  }
  
  observeEvent(input$save_note, {
    rng <- pending_range()
    
    row <- tibble::tibble(
      cdec_id      = input$station,
      datetime_start = as.character(format(rng[1], "%Y-%m-%d %H:%M")),
      datetime_end   = as.character(format(rng[2], "%Y-%m-%d %H:%M")),
      reason    = input$flag_type,
      notes     = input$note_detail,
      warmup_vals = input$warmup,
      lower_limit = input$physical_limits[1],
      upper_limit = input$physical_limits[2],
      consec_threshold = input$consec_thr,
      stuck_threshold = input$stuck_thr,
      mad_mult = input$k,
      jump_threshold = input$j,
      logged_by = input$person_logging,
      logged_at = as.character(format(Sys.time(), "%Y-%m-%d %H:%M"))
    )
    
    # add to google sheets
    sheet_append(ss = sheet_url, sheet = "qc_notes", data = row)
    removeModal()
  })
  
   ### Percent flagged -------------
  output$percent_flagged <- renderDT({
    df <- flagged_df() |> 
      mutate(n_total = n()) |> 
      group_by(flag_ann, n_total) |> 
      summarize(n_flagged = n()) |> 
      ungroup() |> 
      mutate(pct_flagged = n_flagged/n_total) |> 
      # mutate(flag_ann = forcats::fct_relevel(flag_ann, "good")) |> 
      select(flag_ann, n_flagged, pct_flagged) |> 
      # arrange(flag_ann) |> 
      filter(!flag_ann %in% c("good", "warm-up"))
    
    df_w_total <- df |> janitor::adorn_totals(where = "row", fill = "-", na.rm = TRUE)
    
    datatable(df_w_total,
              rownames = FALSE,
              options = list(
                fixedHeader=TRUE,
                autoWidth = TRUE,
                pageLength = 10,
                scrollCollapse = TRUE),
              colnames = c("Flag type", 
                           "Flagged values", 
                           "Percent flagged")) |> 
      formatPercentage(columns = "pct_flagged",
                     digits = 2) |> 
      formatRound("n_flagged", digits = 0, mark = ",") |> 
      formatStyle(1:ncol(df_w_total),
                  target='row',
                  fontWeight = styleEqual('Total', "bold")
      )
        })
  
  ### Table of flagged values --------------
  output$table_flagged <- renderDT({
    df <- flagged_df() |> filter(flag) |> select(-flag)
    datatable(df,
              rownames = FALSE,
              filter = "top",
              options = list(
                autoWidth = TRUE,
                pageLength = 10),
              ,
              colnames = c("CDEC ID", 
                           "Datetime", 
                           "Month",
                           "EC",
                           "Flag type")) 
  })
  
 
  
  
  ## Tab 2 -----------
  ### Station summary -----------
  output$station_summary <- renderDT({
    df <- filtered_station() |> 
      filter(!is.na(datetime)) |> 
      mutate(step = abs(parameter_value - lag(parameter_value))) |>
      filter(parameter_value > 0, parameter_value < 50000, lag(parameter_value) > 0) |>
      group_by(month = month(datetime)) |>
      summarise(step_p999 = round(quantile(step, .999, na.rm = TRUE),1), step_max = max(step, na.rm = TRUE),
                val_p001 = round(quantile(parameter_value, 0.001, na.rm=TRUE),1),
                val_p999 = round(quantile(parameter_value, .999, na.rm = TRUE),1), 
                val_min = min(parameter_value, na.rm = TRUE),
                val_max = max(parameter_value, na.rm = TRUE)) |> 
      ungroup() 
    
    datatable(df,
              rownames = FALSE,
              options = list(
                autoWidth = TRUE,
                pageLength = 12),
              colnames = c("Month", 
                           "Step (0.999 quantile)", 
                           "Max Step",
                           "EC (0.001 quantile)",
                           "EC (0.999 quantile)",
                           "Min EC",
                           "Max EC")
    )
    
    
  })
  
  ### descriptive plots --------------------
  output$histogram <- renderPlotly(
    filtered_station() |> 
      ggplot() + 
      geom_histogram(aes(parameter_value), fill = "steelblue", color = "black") +
      labs(x = "EC", y = "Count")+
      theme_bw()
  )
  
  output$boxplot <- renderPlotly(
    filtered_station() |> 
      filter(!is.na(datetime)) |> 
      ggplot() + 
      geom_boxplot(aes(x = factor(month), y = parameter_value, fill = factor(month)), color = "black") +
      labs(x = "Month", y = "EC")+
      theme_bw()
  )
  
  
  ## Map ---------------
  output$map <- renderLeaflet({
    leaflet() |> 
      addProviderTiles(providers$CartoDB.Positron) |> 
      fitBounds(
        lng1 = min(stations$Longitude), lat1 = min(stations$Latitude),
        lng2 = max(stations$Longitude), lat2 = max(stations$Latitude),
        options = list(padding = c(20, 20))
      ) |>
      addLabelOnlyMarkers(data = stations,
                          lat = ~Latitude,
                          lng = ~Longitude,
                          label = ~cdec_id,
                          labelOptions = labelOptions(
                            noHide = TRUE,
                            direction = "top",
                            textOnly = TRUE
                          )
      )|> 
      addCircleMarkers(
        data = stations,
        lat = ~Latitude,
        lng = ~Longitude,
        popup = ~paste0(cdec_id, " | ", station_id, ": ", description),
        stroke = TRUE,
        weight = 2,
        radius = 6,
        fillOpacity = 0.6
      ) |>
      addScaleBar(position = "bottomright")
    
    
  })
}