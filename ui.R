
ui <- page_navbar(
  title = HTML("conductivity-flagger <span style='font-size: 0.7em; opacity: 0.7;'> Version 0.2</span>"),
  window_title = "conductivity-flagger",
  theme = bs_theme(bootswatch = "sandstone"),
  header = tagList(
    tags$style(HTML("
  .navbar-brand { border-right: 2px solid #dee2e6; padding-right: 20px; margin-right: 10px; }
  .nav-link.active { background-color: #CFE2ED !important; border-radius: 4px; }
  .nav-link { padding: 8px 16px !important; }
  .plot-border {border: 2px solid #dee2e6; border-radius:6px; padding:12px;}
  .dygraph-axis-label-y2 { display: none !important; }
")),
    tags$div(
      style = "background-color: #f8f9fa; padding: 10px; border-bottom: 1px solid #ddd;",
      p("Contact FlowWest for questions or comments: Catarina Pien",
        tags$a(href = "mailto:cpien@flowwest.com", "cpien@flowwest.com"),
        style = "margin: 4px 0 0 0 ; font-size: 15px;")
    )
  ),
  
  ### Plots and Summary --------------------
  nav_panel(
    "Check Data",
    layout_sidebar(
      #### Sidebar ------------
      sidebar = sidebar(
        width = 350,
        div(
          style = "border: 1px solid #dee2e6; border-radius: 6px; padding: 12px; margin-bottom: 12px;",
          h5("Select Data Filters"),
          selectInput("station", 
                      label = "Select station:",
                      choices = sort(unique(stations$cdec_id)),
                      multiple = FALSE, 
                      selected = "DMC"),
          dateInput("start_date", "Select Start date:", value = Sys.Date() - (365*2)),
          dateInput("end_date",   "Select End date:",   value = Sys.Date() -365 )
          ),
        div(
          style = "border: 1px solid #dee2e6; border-radius: 6px; padding: 12px; margin-bottom: 12px;",
          h5("QC Filter Settings"),
          sliderInput(
            "physical_limits",
            "Select physical minimum and maximum",
            value = c(1, 1503),
            min = 1, 
            max = 50000),
          sliderInput(
            "stuck_thr",
            "Select threshold for stuck sensor (15-min intervals):",
            value = 16,
            min = 4,
            max = 96),
          sliderInput(
            "k",
            "Select threshold for MAD multiplier k:",
            value = 8,
            min = 1,
            max = 12),
          sliderInput(
          "j",
          "Select threshold for acceptable jump between values:",
          value = 321,
          min = 1,
          max = 10000)
        ),
        div(
          style = "border: 1px solid #dee2e6; border-radius: 6px; padding: 12px;",
          actionButton("submit", "Submit", class = "btn-primary w-100")
        )
      ),
      
      #### Plots ----------------
      navset_tab(
        nav_panel(
          "Data Flagging",
          br(),
          div(
            style = "text-align: center;", 
            h3(textOutput("station_text"))
          ),
          br(),
          withSpinner(dygraphOutput("original_plot", width = "90%")),
          hr(),
          tags$div(
            style = "display:flex; flex-wrap:wrap; justify-content:center; gap:16px; align-items:center; margin-bottom:8px; font-size:14px; color:#555;",
            tags$div(
              style = "display:flex; align-items:center; gap:6px;",
              tags$span(style = "display:inline-block; width:8px; height:8px; border-radius:50%; background-color: rgba(153,153,153,0.3);"),
              "good"
            ),
            tags$div(
              style = "display:flex; align-items:center; gap:6px;",
              tags$span(style = "display:inline-block; width:12px; height:12px; border-radius:50%; background-color: rgba(232, 109, 176,0.9);"),
              "warm-up"
            ),
            tags$div(
              style = "display:flex; align-items:center; gap:6px;",
              tags$span(style = "display:inline-block; width:12px; height:12px; border-radius:50%; background-color: rgba(168, 15, 103,0.9);"),
              "warm-up & extreme"
            ),
            tags$div(
              style = "display:flex; align-items:center; gap:6px;",
              tags$span(style = "display:inline-block; width:12px; height:12px; border-radius:50%; background-color: rgba(230,159,0,0.9);"),
              "extreme value"
            ),
            tags$div(
              style = "display:flex; align-items:center; gap:6px;",
              tags$span(style = "display:inline-block; width:12px; height:12px; border-radius:50%; background-color: rgba(0,114,178,0.9);"),
              "stuck value"
            ),
            tags$div(
              style = "display:flex; align-items:center; gap:6px;",
              tags$span(style = "display:inline-block; width:12px; height:12px; border-radius:50%; background-color: rgba(0,158,115,0.9);"),
              "outlier/spike"
            )
          ),
          tags$div(id = "qc_tooltip",
                   style = "position: fixed; z-index: 1000; background: white; border: 1px solid #ccc;
                   border-radius: 4px; padding: 6px 10px; font-size: 13px;
                   box-shadow: 0 1px 4px rgba(0,0,0,0.2); pointer-events: none; display: none;"),
          withSpinner(dygraphOutput("qc_plot", width = "90%")),
          hr(),
          
          h4("Percent Flagged"),
          withSpinner(DTOutput("percent_flagged")),
          hr(),
          h4("Flagged Values"),
          withSpinner(DTOutput("table_flagged"))
        ),
        

        
        ### Data Summary -------------------
        nav_panel(
          "Data Diagnostics",
          br(),
          div(
            p("This data on this tab reflect summaries across the whole downloaded timespan of the dataset 
              (1/1/2020 - 12/31/2025) rather than just the selected date range of the data"),
            hr(),
          DTOutput("station_summary"),
          ),
          hr(),
          div(
          withSpinner(plotlyOutput("histogram"))
          ),
          hr(),
          div(
            withSpinner(plotlyOutput("boxplot"))
          )
        )
      )
    )
  ),
  
  ### Map ------------------------
  nav_panel(
    "Station Map",
    leafletOutput("map", height = "700px")
    
  )
)