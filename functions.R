hampel_flag <- function(candidate, good_values, k = 8) {
  spread <- mad(good_values)
  center <- median(good_values)
  threshold <- max(center * .1, spread * k)
  lower_bound <- center - threshold
  upper_bound <- center + threshold
  return(candidate > upper_bound || candidate < lower_bound)
}

flagger <- function(x, j, k = 8, warmup = 12, stuck_threshold = 8, consec_threshold = 24, physical_min = 10, physical_max = 5000) {
  flag <- logical(length(x))
  good <- numeric(0)
  prev_raw <- NA_real_
  consec <- 0
  repeat_count <- 0
  
  for (i in seq_along(x)) {
    
    # flag NAs
    if(is.na(x[i])) {
      flag[i] <- TRUE
      consec <- consec + 1
      next
    }
    
    # check for repeat values
    if(!is.na(prev_raw) && round(x[i], 1) == round(prev_raw, 1)) {
      repeat_count <- repeat_count + 1
    } else {
      repeat_count <- 0
    }
    
    # reset good values if too many bad values in a row
    if(repeat_count == 0 & consec > consec_threshold) {
      good <- numeric(0) 
      consec <- 0
    }
    
    # seed good values
    if(length(good) < warmup) {
      if (x[i] <= physical_min || x[i] >= physical_max) {
        flag[i] <- TRUE
        consec <- consec + 1
        repeat_count <- 0
        next
      } else {
        good <- c(good, x[i])
        flag[i] <- FALSE 
        prev_raw <- x[i]
        consec <- 0
        next
      }
    } 
    
    # check for extreme values
    if (x[i] <= physical_min || x[i] >= physical_max) {
      flag[i] <- TRUE
      consec <- consec + 1
      repeat_count <- 0
      next
    }
    
    # flag stuck sensor
    if (repeat_count > stuck_threshold) {
      flag[i] <- TRUE
      consec <- consec + 1
      prev_raw <- x[i]
      next
    }
    
    # stats flag
    suspect <- hampel_flag(x[i], good, k)
    jump <- abs(prev_raw - x[i]) > j
    if(suspect & jump ) {
      flag[i] <- TRUE
      consec <- consec + 1
    } else {
      good <- tail(c(good, x[i]), 24)
      flag[i] <- FALSE
      consec <- 0
      prev_raw <- x[i]
    }
  }
  return(flag)
}


flagger2 <- function(x, j, k = 8, warmup = 12, stuck_threshold = 8, consec_threshold = 24, physical_min = 10, physical_max = 5000) {
  flag <- logical(length(x))
  annotation <- character(length(x))
  good <- numeric(0)
  prev_raw <- NA_real_
  consec <- 0
  repeat_count <- 0
  
  for (i in seq_along(x)) {
    
    # flag NAs
    if(is.na(x[i])) {
      flag[i] <- TRUE
      annotation[i] <- "NA"
      consec <- consec + 1
      next
    }
    
    # check for repeat values
    if(!is.na(prev_raw) && round(x[i], 1) == round(prev_raw, 1)) {
      repeat_count <- repeat_count + 1
    } else {
      repeat_count <- 0
    }
    
    # reset good values if too many bad values in a row
    if(repeat_count == 0 & consec > consec_threshold) {
      good <- numeric(0)
      consec <- 0
    }
    
    # seed good values
    if(length(good) < warmup) {
      if (x[i] <= physical_min || x[i] >= physical_max) {
        flag[i] <- TRUE
        annotation[i] <- "warm-up extreme value"
        consec <- consec + 1
        repeat_count <- 0
        next
      } else {
        good <- c(good, x[i])
        flag[i] <- FALSE
        annotation[i] <- "warm-up"
        prev_raw <- x[i]
        consec <- 0
        next
      }
    }
    
    # check for extreme values
    if (x[i] <= physical_min || x[i] >= physical_max) {
      flag[i] <- TRUE
      annotation[i] <- "extreme value"
      consec <- consec + 1
      repeat_count <- 0
      next
    }
    
    # flag stuck sensor
    if (repeat_count > stuck_threshold) {
      flag[i] <- TRUE
      annotation[i] <- "stuck value"
      consec <- consec + 1
      prev_raw <- x[i]
      next
    }
    
    # stats flag
    suspect <- hampel_flag(x[i], good, k)
    jump <- abs(prev_raw - x[i]) > j
    if(suspect & jump ) {
      flag[i] <- TRUE
      annotation[i] <- "hampel & jump"
      consec <- consec + 1
    } else {
      good <- tail(c(good, x[i]), 24)
      flag[i] <- FALSE
      annotation[i] <- "good"
      consec <- 0
      prev_raw <- x[i]
    }
  }
  return(list(flag=flag, annotation=annotation))
}