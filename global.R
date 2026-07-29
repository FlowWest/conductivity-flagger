source("app_config.R") # Change this to USE_PRECOMPUTED_FLAGS to TRUE to use pre-flagged data based on defaults

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
library(duckdb)
library(DBI)
source("functions.R")
library(googlesheets4)

# Right now BKS is in the EC dataset but haven't set up how the flagger will work on it yet so leaving it out of selection
stations <- read.csv("data/stations.csv")|> filter(!cdec_id %in% c("CCS", "BKS"))

## EC data ------------------
# Parquet file queried via duckdb connection reduces the memory needing to be stored for the whole EC dataset. 
# Only requested station is pulled in at a time.
ec_parquet <- "data/ec_2020_2025.parquet"
ec_con <- dbConnect(duckdb::duckdb(shared_home = FALSE, allow_extensions = FALSE), read_only = TRUE)

# duckdb doesn't preserve "America/Los_Angeles" tzone label (it comes back tagged
# "UTC") so reformatted to work with original script
get_station_data <- function(station) {
  dbGetQuery(ec_con, sprintf("
    SELECT location_id, datetime, parameter_value
    FROM read_parquet('%s')
    WHERE location_id = ?
    ORDER BY datetime
  ", ec_parquet), params = list(station)) |>
    as_tibble() |>
    mutate(datetime = with_tz(datetime, "America/Los_Angeles"),
           month = month(datetime))
}

# Precomputed alternative to get_station_data() -- used when USE_PRECOMPUTED_FLAGS
# is TRUE, so the flagger doesn't need to run on every Submit. 
flagged_parquet <- "data/flagged_allstations.parquet"

get_flagged_station_data <- function(station) {
  dbGetQuery(ec_con, sprintf("
    SELECT location_id, datetime, parameter_value, flag, flag_ann
    FROM read_parquet('%s')
    WHERE location_id = ?
    ORDER BY datetime
  ", flagged_parquet), params = list(station)) |>
    as_tibble() |>
    mutate(datetime = with_tz(datetime, "America/Los_Angeles"),
           month = month(datetime))
}

ec_date_range <- dbGetQuery(ec_con, sprintf(
  "SELECT MIN(datetime) AS min_dt, MAX(datetime) AS max_dt FROM read_parquet('%s')", ec_parquet))
first_date <- format(with_tz(ec_date_range$min_dt, "America/Los_Angeles"), "%Y-%m-%d")
last_date <- format(with_tz(ec_date_range$max_dt, "America/Los_Angeles"), "%Y-%m-%d")

onStop(function() {
  dbDisconnect(ec_con, shutdown = TRUE)
})

## Info text for using plot----------------------
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

## Google sheets -----------
gs4_auth(cache = ".secrets", email = "cpien@flowwest.com")
sheet_url = "https://docs.google.com/spreadsheets/d/1zFra8tJ7b7OsU7ML-76W6CurTvRTi2OAwj75SAioUcI/edit?gid=0#gid=0"
