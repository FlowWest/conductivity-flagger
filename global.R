library(shiny)
library(tidyverse)
library(bslib)
library(plotly)
library(shinycssloaders)
library(DT)
library(leaflet)
library(plotly)
library(dygraphs)
library(xts)
library(htmlwidgets)
library(shinycssloaders)
library(jsonlite)
library(janitor)
source("functions.R")
library(googlesheets4)

# Right now BKS is in the EC dataset but haven't set up how the flagger will work on it yet so leaving it out of selection
stations <- read.csv("data/stations.csv")|> filter(!cdec_id %in% c("CCS", "BKS"))
ec_data <- read_rds("data/ec_2020_2025.rds")|> mutate(month = month(datetime))  |> 
  select(location_id, datetime, month, parameter_value) |> arrange(datetime)
first_date = format(min(ec_data$datetime, na.rm = TRUE), "%Y-%m-%d")
last_date = format(max(ec_data$datetime, na.rm = TRUE), "%Y-%m-%d")


plot_help <- function(text = paste0(
  "<strong>Plot Controls</strong><br>",
  "<ul><li>Hover to see the value details</li>
                        <li>Click and drag a box around interested values to zoom in on an area</li>
                        <li>Double-click to reset plot</li>
                        <li>Zooming on the plot will also highlight the zoomed date window in the above plot</li>
                        <li>To pan once you are zoomed in, hold Shift while clicking and dragging left or right</li></ul><br>",
  "<strong>Annotations</strong><br>",
  "<ul><li>Check the <strong>Make an annotation</strong> box to annotate a point or series of points</li>
  <li>Once the box is checked, zoom in on several points or click on one point to annotate</li></ul>")) {
  bslib::popover(
    trigger = shiny::icon("circle-question", class = "text-muted"),
    shiny::HTML(text),
    title = "Plot controls"
  )
}


### Google sheets -----------
gs4_auth(cache = ".secrets", email = "cpien@flowwest.com")
sheet_url = "https://docs.google.com/spreadsheets/d/1zFra8tJ7b7OsU7ML-76W6CurTvRTi2OAwj75SAioUcI/edit?gid=0#gid=0"
