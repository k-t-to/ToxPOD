if(!require("splines")) install.packages("splines")

# Parse dose response data -----
# Function to check input data format
data_checks <- function(dat, dr_threshold) {
  # Data should contain only two columns 
  if (ncol(dat) != 2) {
    stop("Data should contain 2 columns")
  }
  # Doses and responses should be numeric
  if (any(apply(dat, 2, class) != "numeric")) {
    stop("Doses and responses should be numeric")
  }
  # Need at least 4 doses 
  n_doses <- length(unique(dat[,1]))
  if (n_doses < 4) {
    stop("At least 4 doses required for spline interpolation")
  }
  
  # Want doses to have more than one response
  # Number of doses
  dose_check <- ceiling(n_doses * dr_threshold)
  if (sum(table(dat[,1]) >= 3) < dose_check) {
    stop(paste0("Insufficient Data. At least ", 
                dose_check, 
                " doses require at least 3 responses"))
  }
}

# Function to read in data
parse_data <- function(data_file_path, dr_threshold = 0.8) {
  # Read in data
  dat <- read.table(data_file_path, header = T)
  # Check data format
  data_checks(dat, dr_threshold = dr_threshold)
  # Convert doses to log10 scale
  colnames(dat) <- c("dose", "response")
  dat$log10_dose <- log10(dat$dose + 1)
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
                                    quantile_probs = c(0.05, 0.5, 0.95)) {
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
  # Add back original dose
  spline_values$dose <- (10^spline_values$log10_dose) - 1
  spline_values <- spline_values[,c("bs_index", "dose", "log10_dose", "response_pred")]
  
  # Extract Menger Curvature calculations
  mc_values <- lapply(names(pods), function(bs_index){
    temp <- pods[[bs_index]]$mc_values
    temp <- data.frame("bs_index" = as.numeric(bs_index), temp)
    return(temp)
  })
  mc_values <- do.call("rbind", mc_values)
  rownames(mc_values) <- NULL
  # Add back original dose
  mc_values$dose <- (10^mc_values$log10_dose) - 1
  mc_values <- mc_values[,c("bs_index", "dose", "log10_dose", "mc")]
  
  pod_values <- lapply(names(pods), function(bs_index) {
    temp <- pods[[bs_index]]$POD
    temp <- data.frame("bs_index" = as.numeric(bs_index), temp)
    return(temp)
  })
  pod_values <- do.call("rbind", pod_values)
  rownames(pod_values) <- NULL
  # Add back original dose
  pod_values$dose <- (10^pod_values$log10_dose) - 1
  pod_values <- pod_values[,c("bs_index", "dose", "log10_dose", "mc")]
  
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
