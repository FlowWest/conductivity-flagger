library("CDECRetrieve")
library("tidyverse")

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

