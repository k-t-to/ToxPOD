if(!require("splines")) install.packages("splines")

### General -----
rescale <- function(original, old_min = NULL, old_max = NULL, new_min, new_max) {
  if (is.null(old_min)) old_min <- min(original)
  if (is.null(old_max)) old_max <- max(original)

  # Append old min and max to ensure proper scaling
  original <- c(old_min, old_max, original)

  scale_val <- (new_max - new_min)/(old_max - old_min)
  new_values <- (scale_val * (original - old_min)) + new_min

    # Remove appended values
  new_values[-c(1:2)]
}

### Prepare Data for Analysis -----
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

  # Rescale 0 dose
  if (any(dat$dose == 0)) {
    min_val <- min(dat$dose[dat$dose != 0])
    new_zero <- log10(min_val/10)
    dat$log10_dose[dat$dose == 0] <- new_zero
  }

  dat[,c("dose", "log10_dose", "response")]
}

calculate_interpolated_doses <- function(dose_vector, length.out = 50) {
  start_value <- min(dose_vector)
  end_value <- max(dose_vector)
  new_doses <- seq(from = start_value, to = end_value, length.out = length.out)
}

### Curvature -----
calculate_menger_curvature <- function(interpolated_dose_vector,
                                       predicted_response_vector) {
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

# Calculate sequential Menger Curvatures and return the max
calculate_pod_from_menger_curvature <- function(predicted_dose_response) {
  # Declare list to store calculations
  n_3 <- length(predicted_dose_response$x) - 2
  MC_values <- list(
    log10_dose = vector("double", n_3),
    mc         = vector("double", n_3)
  )

  # Loop through data and calculate MC
  for (i in 1:n_3) {
    end <- i + 2
    dose_temp <- predicted_dose_response$x[i:end]
    response_temp <- predicted_dose_response$y[i:end]
    MC_temp <- calculate_menger_curvature(
      interpolated_dose_vector  = dose_temp,
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
    POD       = pods # Log10 scale
  )
}

### POD Estimation -----
perform_bootstrap <- function(dat_list, interpolated_doses, bs_id) {
  # Sample 1 response from each dose group
  bootstrap_responses <- lapply(dat_list,
                                function(x) x[sample(nrow(x),1),])
  bootstrap_responses <- do.call("rbind", bootstrap_responses)

  # Create spline model
  bs_spline_model <- splines::interpSpline(bootstrap_responses[,"log10_dose"],
                                           bootstrap_responses[,"response"])

  # Predict responses for interpolated doses
  pred_vals <- predict(bs_spline_model, interpolated_doses)

  # Get POD
  mc_pod <- calculate_pod_from_menger_curvature(pred_vals)

  # Combine spline prediction and curvature
  spline_predictions <- data.frame(bs_index      = bs_id,
                                   log10_dose    = pred_vals$x,
                                   response_pred = pred_vals$y,
                                   row.names     = NULL)
  spline_predictions$mc <- mc_pod$MC_values$mc[match(spline_predictions$log10_dose,
                                                     mc_pod$MC_values$log10_dose)]

  list(
    bootstrap_values   = data.frame(bs_index  = bs_id,
                                    bootstrap_responses,
                                    row.names = NULL),
    spline_predictions = spline_predictions,
    POD                = data.frame(bs_index  = bs_id,
                                    mc_pod$POD,
                                    row.names = NULL)
  )
}

calculate_pod_estimates <- function(dat,
                                    seed,
                                    resample_size = 1000,
                                    interpolation_size = 50) {
  # Get original doses
  doses_raw <- sort(unique(dat$dose))

  # Calculate doses for spline interpolation
  interpolated_doses <- calculate_interpolated_doses(dose_vector = unique(dat$log10_dose),
                                                     length.out  = interpolation_size)

  # Perform bootstrap POD estimation
  dat <- split(dat, dat[,"dose"])
  set.seed(seed)
  pods <- lapply(1:resample_size,
                 function(x) perform_bootstrap(dat, interpolated_doses, bs_id = x))

  # Extract resample values
  bs_values <- lapply(pods, function(x) x$bootstrap_values)
  bs_values <- do.call("rbind", bs_values)

  # Extract spline predictions
  spline_values <- lapply(pods, function(x) x$spline_predictions)
  spline_values <- do.call("rbind", spline_values)

  # Extract log10 PODs
  pod_values <- lapply(pods, function(x) x$POD)
  pod_values <- do.call("rbind", pod_values)

  # Back transform doses and PODs
  if (doses_raw[1] == 0) {
    doses_temp <- 10^interpolated_doses

    # ID interpolated doses within (0, min_dose)
    is_small <- doses_temp <= doses_raw[2]
    doses_small <- doses_temp[is_small]
    # Rescale values between 0 and min dose to be left-bound by 0
    old_min <- doses_raw[2]/10
    old_max <- doses_raw[2]
    doses_small <- rescale(doses_small,
                           old_min = old_min,
                           old_max = old_max,
                           new_min = 0,
                           new_max = old_max)
    # Replace with rescaled values
    doses_temp[is_small] <- doses_small

    # Add back-transformed doses
    spline_values$dose <- doses_temp[match(spline_values$log10_dose, interpolated_doses)]

    # ID PODs within (0, min_dose)
    pod_doses <- 10^pod_values$log10_dose
    is_small <- pod_doses <= doses_raw[2]
    if (any(is_small)){
      pod_small <- pod_doses[is_small]
      # Rescale values between 0 and min dose to be left-bound by 0
      pod_small <- rescale(pod_small,
                           old_min = old_min,
                           old_max = old_max,
                           new_min = 0,
                           new_max = old_max)
      # Replace with rescaled values
      pod_doses[is_small] <- pod_small
    }
    # Add back-transformed pods
    pod_values$dose <- pod_doses
  } else {
    spline_values$dose <- 10^(spline_values$log10_dose)
    pod_values$dose <- 10^(pod_values$log10_dose)
  }

  # Organize columns
  spline_values <- spline_values[,c("bs_index", "dose", "log10_dose", "response_pred", "mc")]
  pod_values <- pod_values[,c("bs_index", "dose", "log10_dose", "mc")]

  list(
    "bootstrap_values"   = bs_values,
    "spline_predictions" = spline_values,
    "pods"               = pod_values
  )
}