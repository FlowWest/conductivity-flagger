# conductivity-flagger

Internal app to optimize conductivity (EC) flagging

## Methods 

- Data are downloaded from CDEC (2020-2025) from 34 stations
- `flagger` function run on all stations
- Settings include:
  - number of **warmup** values used to produce a new baseline
  - **consecutive erroneous flagged data** needed in order to require a new baseline generation
  - **physical min and max** for each station to cut off extreme outliers
  - continuous **stuck** (repeating) values
  - **k** and **j** for hampel filter to determine MAD multiplier and jump threshold for outliers 
  
## App includes:
- data flagging and viewing tools
- table of flagged data and % flagged
- episode viewers to zoom in on hampel events and evaluate whether the hampel filter is necessary
- summaries of the data used to determine flag parameters and to view data patterns across the 5 years
- map of stations

## Deploying app

### Internal version

Allows adjusting QC flags parameters

<https://flowwest.shinyapps.io/conductivity-flagger-internal/> Turn app_config to `USE_PRECOMPUTED_FLAGS <- FALSE`

### Demo version

Uses default flag parameters - preloads flagged data, speeding app up

<https://flowwest.shinyapps.io/conductivity-flagger-demo/> Turn app_config to `USE_PRECOMPUTED_FLAGS <- TRUE`
