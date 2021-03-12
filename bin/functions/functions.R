if(!require("splines")) install.packages("splines")

# Parse dose response data -----

# Function to read in data
parse_data <- function(in_val, in_type) {
  # Read in data
  if (in_type == "file") {
    dat <- read.table(in_val, header = T)
  } else if (in_type == "str") {
    dat <- read.table(text = in_val, header = T)
  }
  
  # Check data format
  # Data should contain only two columns 
  if (ncol(dat) != 2) {
    stop("Data should contain 2 columns")
  }
  
  # Remove rows with missing data
  dat <- dat[!is.na(dat[,1]),]
  dat <- dat[!is.na(dat[,2]),]
  
  # Doses and responses should be numeric
  if (any(apply(dat, 2, function(x) !is.numeric(x)))) {
    stop("Doses and responses should be numeric")
  }
  
  # Rename columns of data
  colnames(dat) <- c("dose", "response")

  # Remove doses with fewer than three replicates
  dose_rm <- as.numeric(names(which(table(dat[,1]) < 3)))
  if (length(dose_rm) > 0) {
    warning(paste0("Minimum of 3 replicates per dose. Removing doses from dataset: ", 
                   paste(dose_rm, collapse = ", ")))
    dat <- dat[!dat[,1] %in% dose_rm,]
  }
  
  # Need at least 4 doses 
  n_doses <- length(unique(dat[,1]))
  if (n_doses < 4) {
    stop("At least 4 doses with 3 replicates required for spline interpolation")
  }
  
  # Convert doses to log10 scale
  dat$log10_dose <- log10(dat$dose)
  
  if (any(dat$dose == 0)) {
    min_val <- min(dat$dose[dat$dose != 0])
    new_zero <- log10(min_val/10)
    dat$log10_dose[dat$dose == 0] <- new_zero
  }
  
  dat <- dat[,c("dose", "log10_dose", "response")]
  # Format data as list
  dat <- split(dat, dat[,1])
  dat
}

# Generate interpolated doses -----
calculate_interpolated_doses <- function(dose_vector, length.out = 50) {
  start_value <- min(dose_vector)
  end_value <- max(dose_vector)
  new_doses <- seq(from = start_value, to = end_value, length.out = length.out)
  new_doses
}

# Menger Curvature functions -----
calculate_menger_curvature <- function(interpolated_dose_vector, predicted_response_vector) {
  # Area calculation for numerator
  matrix_temp <- cbind(interpolated_dose_vector, predicted_response_vector, 1)
  A <- 0.5 * abs(det(matrix_temp))
  numerator <- 4 * A
  
  # Distance calculation for denominator
  line_1 <- dist(matrix_temp[c(1, 2), -3])
  line_2 <- dist(matrix_temp[c(2, 3), -3])
  line_3 <- dist(matrix_temp[c(1, 3), -3])
  denominator <- line_1 * line_2 * line_3
  
  as.numeric(numerator / denominator)
}

# Calculate sequential Menger Curvatures and return the max -----
calculate_pod_from_menger_curvature <- function(predicted_dose_response) {
  # Declare list to store calculations
  n_3 <- length(predicted_dose_response$x) - 2
  MC_values <- list(
    log10_dose = vector("double", n_3),
    mc = vector("double", n_3)
  )
  # Loop through data and calculate MC
  for (i in 1:n_3) {
    end <- i + 2
    dose_temp <- predicted_dose_response$x[i:end]
    response_temp <- predicted_dose_response$y[i:end]
    MC_temp <- calculate_menger_curvature(
      interpolated_dose_vector = dose_temp,
      predicted_response_vector = response_temp
    )
    MC_values[["log10_dose"]][i] <- dose_temp[2]
    MC_values[["mc"]][i] <- MC_temp
  }
  # Return the dose corresponding to the highest curvature.
  MC_values <- data.frame(MC_values)
  pods <- MC_values[which.max(MC_values$mc),]
  list(
    MC_values = MC_values,
    POD = pods
  )
}


perform_bootstrap <- function(dat, interpolated_doses) {
  bootstrap_responses <- lapply(dat, 
                               function(x) x[sample(nrow(x),1),])
  bootstrap_responses <- do.call("rbind", bootstrap_responses)
  # Create spline model
  bs_spline_model <- splines::interpSpline(bootstrap_responses[,2], 
                                           bootstrap_responses[,3])
  # Predict responses for interpolated doses
  pred_vals <- predict(bs_spline_model, interpolated_doses)
  # Get POD
  mc_pod <- calculate_pod_from_menger_curvature(pred_vals)
  
  # Output
  list(
    bootstrap_values = bootstrap_responses,
    spline_predictions = data.frame(log10_dose = pred_vals$x, response_pred = pred_vals$y),
    mc_values = mc_pod$MC_values,
    POD = mc_pod$POD
  )
  
}

calculate_pod_quantiles <- function(dat,
                                    resample_size = 1000,
                                    interpolation_size = 50,
                                    quantile_probs = c(0.025, 0.5, 0.975)) {
  dose_vector <- sapply(dat, function(x) x$log10_dose[1])
  interpolated_doses <- calculate_interpolated_doses(dose_vector = dose_vector, 
                                                     length.out = interpolation_size)

  pods <- lapply(1:resample_size, function(x) perform_bootstrap(dat, interpolated_doses))
  # Reformat pods
  names(pods) <- 1:length(pods)
  # Extract resample values
  bs_values <- lapply(names(pods), function(bs_index){
    temp <- pods[[bs_index]]$bootstrap_values
    temp <- data.frame("bs_index" = as.numeric(bs_index), temp)
    return(temp)
  })
  bs_values <- do.call("rbind", bs_values)
  rownames(bs_values) <- NULL
  
  # Extract spline predictions
  spline_values <- lapply(names(pods), function(bs_index){
    temp <- pods[[bs_index]]$spline_predictions
    temp <- data.frame("bs_index" = as.numeric(bs_index), temp)
    return(temp)
  })
  spline_values <- do.call("rbind", spline_values)
  rownames(spline_values) <- NULL

  # Extract Menger Curvature calculations
  mc_values <- lapply(names(pods), function(bs_index){
    temp <- pods[[bs_index]]$mc_values
    temp <- data.frame("bs_index" = as.numeric(bs_index), temp)
    return(temp)
  })
  mc_values <- do.call("rbind", mc_values)
  rownames(mc_values) <- NULL

  pod_values <- lapply(names(pods), function(bs_index) {
    temp <- pods[[bs_index]]$POD
    temp <- data.frame("bs_index" = as.numeric(bs_index), temp)
    return(temp)
  })
  pod_values <- do.call("rbind", pod_values)
  rownames(pod_values) <- NULL

  # Rescale values between 0 and min non-zero dose
  doses_raw <- sort(as.numeric(names(dat)))
  if (doses_raw[1] == 0) {
    doses_temp <- 10^interpolated_doses
    is_small <- doses_temp <= doses_raw[2]
    doses_small <- doses_temp[is_small]
    doses_small <- c(doses_raw[2]/10, doses_raw[2], doses_small)
    doses_small <- doses_raw[2] * ((doses_small - min(doses_small))/(max(doses_small) - min(doses_small)))
    doses_small <- doses_small[-c(1:2)]
    doses_temp[is_small] <- doses_small
    
    spline_values$dose <- doses_temp[match(spline_values$log10_dose, interpolated_doses)]
    mc_values$dose <- doses_temp[match(mc_values$log10_dose, interpolated_doses)]
    
    pod_doses <- 10^pod_values$log10_dose
    is_small <- pod_doses <= doses_raw[2]
    if (any(is_small)){
      pod_small <- pod_doses[is_small]
      pod_small <- c(doses_raw[2]/10, doses_raw[2], pod_small)
      pod_small <- doses_raw[2] * ((pod_small - min(pod_small))/(max(pod_small) - min(pod_small)))
      pod_small <- pod_small[-c(1:2)]
      pod_doses[is_small] <- pod_small
    }
    pod_values$dose <- pod_doses
  } else {
    spline_values$dose <- 10^(spline_values$log10_dose)
    mc_values$dose <- 10^(mc_values$log10_dose)
    pod_values$dose <- 10^(pod_values$log10_dose)
  }
  spline_values <- spline_values[,c("bs_index", "dose", "log10_dose", "response_pred")]
  mc_values <- mc_values[,c("bs_index", "dose", "log10_dose", "mc")]
  pod_values <- pod_values[,c("bs_index", "dose", "log10_dose", "mc")]
  
  quantile_probs <- sort(quantile_probs)
  pod_quantiles <- list(dose = quantile(pod_values$dose, probs = quantile_probs),
                        log10_dose = quantile(pod_values$log10_dose, probs = quantile_probs))
  
  # List output
  list(
    "bootstrap_values" = bs_values,
    "spline_predictions" = spline_values,
    "menger_curvature" = mc_values,
    "pods" = pod_values,
    "pod_quantile" = pod_quantiles
  )
}
