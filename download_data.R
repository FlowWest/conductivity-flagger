library("CDECRetrieve")
library("tidyverse")
library("duckdb")
library("DBI")

# get list of stations
stations <- read.csv("data/stations.csv") |> filter(!cdec_id %in% c("CCS", "BKS"))

# define inputs
sta_list = unique(stations$cdec_id)
start <- "2020-01-01"
end <- "2025-12-31"

# function for inputs
retrieve_stas <- function(sta) {
  cdec_query(sta, 100, "E", start, end)
}

# download data for all stations
event_data <- lapply(sta_list, retrieve_stas) |> 
  bind_rows()

# bdl data - currently not working 
bdl_data <- cdec_query("BDL", 100, "E", start, end)
bdl_data <- cdec_query("BDL", 324, "E", start, end) # spcond

# bks data
bks_data <- cdec_query("BKS", 100, "H", start, end)

# all data 
all_data <- bind_rows(event_data, bks_data)

# write data
write_rds(all_data, "data/ec_2020_2025.rds", compress = "xz")


### Run flagger for demo
source("functions.R")
stations <- read.csv("data/stations.csv")|> filter(!cdec_id %in% c("CCS", "BKS"))
alldata <- readRDS("data/ec_2020_2025.rds")|> filter(!location_id %in% c("CCS", "BKS"))

get_inputs <- function(station) {
  sta_data <- alldata |> filter(location_id == station)
  new_min <- unname(pmax(min(sta_data$parameter_value, na.rm = TRUE) * 0.9, 1))
  new_max <- unname(ceiling(quantile(sta_data$parameter_value, 0.999, na.rm = TRUE) * 1.5))
  
  step_vals <- abs(diff(sta_data$parameter_value))
  new_j <- unname(ceiling(quantile(step_vals, 0.999, na.rm = TRUE)) * 1.25)
  
  output <- list(new_min, new_max, new_j)
  names(output) <- c("min", "max", "j")
  output
}
sta_list <- stations$cdec_id

flag_data <- function(station) {
  sta_data <- alldata |> filter(location_id == station)
  flags <- flagger(x = sta_data$parameter_value,
          warmup = 12,
          j = get_inputs(station)[["j"]],
          k = 8,
          consec_threshold = 24,
          stuck_threshold = 16,
          physical_min = get_inputs(station)[["min"]],
          physical_max = get_inputs(station)[["max"]]
  )
  sta_data |> mutate(flag = flags[["flag"]],
  flag_ann = flags[["annotation"]]
    )
}

flagged <- lapply(sta_list, flag_data)
names(flagged) <- sta_list
flagged_df <- bind_rows(flagged)
saveRDS(flagged_df, "data/flagged_allstations.rds", compress = "xz")
  
# parquet 
flagged_data_sorted <- flagged_df |> arrange(location_id, datetime)
parquet_con <- dbConnect(duckdb())
duckdb_register(parquet_con, "flagged_data_sorted", flagged_data_sorted)
dbExecute(parquet_con, "COPY flagged_data_sorted TO 'data/flagged_allstations.parquet' (FORMAT PARQUET, COMPRESSION ZSTD, ROW_GROUP_SIZE 50000)")
dbDisconnect(parquet_con, shutdown = TRUE)



# write a parquet copy, queries this lazily via duckdb instead
# of loading the whole multi-station table. Sorted by
# location_id/datetime to filter by station
all_data_sorted <- all_data |> arrange(location_id, datetime)
parquet_con <- dbConnect(duckdb())
duckdb_register(parquet_con, "all_data_sorted", all_data_sorted)
dbExecute(parquet_con, "COPY all_data_sorted TO 'data/ec_2020_2025.parquet' (FORMAT PARQUET, COMPRESSION ZSTD, ROW_GROUP_SIZE 50000)")
dbDisconnect(parquet_con, shutdown = TRUE)

