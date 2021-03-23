### General -----
op_def <- par()
op_def <- op_def[!names(op_def) %in% c("cin", "cra", "csi", "cxy", "din", "page")]

# Rounding for display
my_round <- function(x) {
  ifelse(x < 0.01, signif(x, digits = 2), round(x, digits = 2))
}

# Parameters for selected x-axis view option
scale_params <- function(viewopt) {
  if (viewopt == "Original Doses") {
    list(dose_colname = "dose",
         dose_lab = "Dose (Original Scale)",
         pod_lab = "POD Estimates (Original Scale)")
  } else if (viewopt == "Log10(Doses)") {
    list(dose_colname = "log10_dose",
         dose_lab = expression("Dose ("*Log[10]~"Scale)"),
         pod_lab = expression("POD Estimates ("*Log[10]~"Scale)"))
  }
}

# Draw x-axis
my_ax <- function(scale_opts, x_ticks, draw_box = F, ...) {
  axis(1, at = x_ticks[,scale_opts$dose_colname], labels = x_ticks$dose_lab, ...)
  if(!draw_box) box(lwd = 2, bty = "l")
}

# Parameters for analysis results
pod_params <- function(pod_df, in_dat, scale_opts, pod_qs = c(0.025, 0.975),
                       show_median, show_mean) {
  pod_qs <- sort(pod_qs)

  # Create CI legend
  pod_quants <- quantile(pod_df$dose, pod_qs)
  ci_labs <- paste0(pod_qs * 100, "% = ", my_round(pod_quants))
  out_list = list()
  out_list$draw_q_legend <- function(x,y=NULL, ...) {
    legend(x, y, 
           legend = ci_labs,
           col    = "blue",
           lty    = "solid",
           title  = "POD Credible Interval",
           lwd    = 2, 
           xpd    = T,
           ...)
  }
  # Set up location of quantile lines
  out_list$q_l_line <- quantile(pod_df[,scale_opts$dose_colname], pod_qs[1])
  out_list$q_u_line <- quantile(pod_df[,scale_opts$dose_colname], pod_qs[2])
  
  # Create summary legend
  out_list$show_median <- show_median
  out_list$show_mean <- show_mean
  summary_labs <- NULL
  summary_line_locs <- NULL
  if (show_median) {
    summary_labs <- c(summary_labs, paste0("Median = ", my_round(median(pod_df$dose))))
    summary_line_locs <- c(summary_line_locs, median(pod_df[,scale_opts$dose_colname]))
  }
  if (show_mean) {
    mean_val <- mean(pod_df$dose)
    summary_labs <- c(summary_labs, paste0("Mean = ", my_round(mean_val)))
    if (scale_opts$dose_colname == "log10_dose") {
      min_val <- min(in_dat$dose[in_dat$dose != 0])
      if (mean_val < min_val) {
        mean_val <- rescale(mean_val,
                            old_min = min_val/10,
                            old_max = min_val,
                            new_min = 0,
                            new_max = min_val)
      }
      mean_val <- log10(mean_val)
    }
    summary_line_locs <- c(summary_line_locs, mean_val)
  }
  if (show_median | show_mean) {
    # Set up parameters for summary lines
    out_list$summary_line_locs <- summary_line_locs
    out_list$summary_line_types <- c("solid", "dotted")[c(show_median, show_mean)]
    out_list$draw_summary_legend <- function(x, y = NULL, ...) {
      legend(x, y, 
             legend = summary_labs,
             col    = "orange",
             lty    = c("solid", "dotted")[c(show_median, show_mean)],
             lwd    = 2,
             xpd    = T,
             ...)
    }
  }
  out_list
}

# Draw empty plot (for complicated layouts)
blank_plot <- function(x = 0, y = 0, xlim = c(-1,1), ylim = c(-1,1)) {
  plot(x, y, axes = F, ann = F, type = "n", xlim = xlim, ylim = ylim)
}

### Input Data ----- 
plot_input_data <- function(in_dat, scale_opts, x_ticks){
  plot(x           = in_dat[,scale_opts$dose_colname],
       y           = in_dat$response,
       panel.first = grid(),
       xlab        = scale_opts$dose_lab,
       ylab        = "Response",
       main        = "Input Data Type",
       pch         = 16,
       frame.plot  = F,
       yaxt        = "n",
       xaxt        = "n")
  my_ax(scale_opts, x_ticks)
  axis(side = 2, las = 2)
}

# Analysis ----- 
# Distribution of PODs
plot_pod_dist <- function(pod_df, scale_opts, pod_opts, x_ticks) {
  # Calculate density for plotting
  d <- density(pod_df[,scale_opts$dose_colname])

  # Get credible interval shading region
  ql_id <- min(which(d$x >= pod_opts$q_l_line))
  qu_id <- max(which(d$x < pod_opts$q_u_line))
  ql_y1 <- d$y[ql_id]
  qu_y1 <- d$y[qu_id]
  dd <- approxfun(d)

  layout(mat = matrix(c(1,1,1,1,2), nrow = 1))
  par(cex = 1.25, mar = c(5.1,4.1,2.1,1.1))
  plot(d,
       panel.first = grid(),
       xlab        = scale_opts$pod_lab,
       ylab        = "Density",
       xlim        = range(x_ticks[,scale_opts$dose_colname]),
       main        = "Distribution of POD Estimates",
       frame.plot  = F,
       xaxt        = "n")
  my_ax(scale_opts, x_ticks)

  # Fill in credible interval
  polygon(d, col = "gray55")
  segments(x0  = c(pod_opts$q_l_line, pod_opts$q_u_line),
           x1  = c(pod_opts$q_l_line, pod_opts$q_u_line),
           y0  = 0,
           y1  = c(ql_y1, qu_y1),
           lwd = 3,
           col = "blue")
  with(d, polygon(x      = c(x[c(ql_id, ql_id:qu_id, qu_id)]),
                  y      = c(0, y[ql_id: qu_id], 0),
                  col    = "gray25",
                  border = NA))
  par(mar = c(5.1,0.1,4.1,1.1))
  # Add summary lines
  if (pod_opts$show_median | pod_opts$show_mean) {
    y_loc <- dd(pod_opts$summary_line_locs)
    segments(x0  = pod_opts$summary_line_locs,
             x1  = pod_opts$summary_line_locs,
             y0  = 0,
             y1  = y_loc,
             lwd = 3,
             col = "orange",
             lty = pod_opts$summary_line_types)
    blank_plot()
    pod_opts$draw_summary_legend("left", cex = 0.75)
  } else{blank_plot()}

  # Draw CI legend
  pod_opts$draw_q_legend("topleft", cex = 0.75)
  par(op_def)
}

# Bootstrap summary: splines + histogram 
plot_mc_summary <- function(spline_df, pod_df, scale_opts, pod_opts, x_ticks) {
  spline_df$dose <- spline_df[,scale_opts$dose_colname]
  # Summarize predicted responses
  spline_df <- do.call("rbind",by(spline_df, spline_df$dose, function(dat) {
    data.frame(dose     = dat$dose,
               min_resp = min(dat$response_pred), 
               med_resp = median(dat$response_pred), 
               max_resp = max(dat$response_pred))
  }))
  ribbon <- function() {
    grid()
    polygon(x      = c(spline_df$dose, rev(spline_df$dose)),
            y      = c(spline_df$min_resp, rev(spline_df$max_resp)),
            col    = rgb(1,0.75,0.15,0.5),
            border = F)
  }

  layout(cbind(matrix(c(1,1,1,2), nrow = 4, ncol = 5), c(3,3,3,3)))
  # Draw spline summary
  par(mar = c(5.1, 5.1, 4.1, 2.1))
  plot(spline_df$dose,
       spline_df$med_resp,
       panel.first = ribbon(),
       xlab        = scale_opts$dose_lab,
       ylab        = "Predicted Response",
       main        = "Spline Fit Summary",
       cex.main    = 1.5,
       cex.lab     = 1.5,
       cex.axis    = 1.5,
       pch         = 20,
       type        = "b",
       frame.plot  = F,
       xlim        = range(x_ticks[,scale_opts$dose_colname]),
       xaxt        = "n")
  my_ax(scale_opts, x_ticks, cex.axis = 1.5)

  # Draw POD histogram
  par(mar = c(5.1, 5.1, 2.1, 2.1))
  hist(pod_df[,scale_opts$dose_colname],
       panel.first = grid(),
       xlim        = range(x_ticks[,scale_opts$dose_colname]),
       col         = "gray",
       breaks      = unique(spline_df$dose),
       main        = "",
       cex.main    = 1.5,
       cex.lab     = 1.5,
       cex.axis    = 1.5,
       xlab        = scale_opts$pod_lab,
       xaxt        = "n")
  my_ax(scale_opts, x_ticks, cex.axis = 1.5)
  abline(v = c(pod_opts$q_l_line, pod_opts$q_u_line), lwd = 2, col = "blue")
  par(mar = c(5.1,0.6,4.1,2.1))
  if (pod_opts$show_median | pod_opts$show_mean) {
    abline(v   = pod_opts$summary_line_locs,
           lwd = 2, col = "orange",
           lty = pod_opts$summary_line_types)
    blank_plot()
    pod_opts$draw_summary_legend(-1.1,-0.85, cex = 1.2)
  } else{blank_plot()}

  # Add legends
  legend(-1.1, 0.35,
         legend = c("Median Response", "(Min, Max)"),
         col    = c("black", rgb(1,0.75,0.15,0.5)),
         pch    = c(16, NA),
         fill   = c(NA, rgb(1,0.75,0.15,1)),
         border = NA,
         lty    = c("solid", NA),
         xpd    = T,
         cex    = 1.2)
  pod_opts$draw_q_legend(x = -1.1, y = -0.55, cex = 1.2)

  par(op_def); layout(1)
}

### Sample Explorer ----- 
# Draw individual bootstrap plots
plot_mc <- function(spline_df, pod_df, bs_ids, scale_opts, x_ticks) {
  # Get y limits
  y_ax_lims <- range(spline_df$response_pred)
  y_mc_lims <- range(spline_df$mc, na.rm = T)

  # Create plotting layout
  n_bs <- length(bs_ids)
  if(n_bs > 1){
    # Values for actual data plots
    if (n_bs %% 2 == 0) {
      loc_vals <- 1:n_bs
      n_loc <- n_bs
    } else {
      # Adjust last value for odd numbered selection
      loc_vals <- c(1:n_bs, (n_bs + 4))
      n_loc <- n_bs + 1
    }

    # Left column of data plots
    l_vals <- loc_vals[seq(1,n_loc,2)]
    l_mat <- matrix(rep(l_vals, each = 16), ncol = 4, byrow = T)
    
    # Right column of data plots 
    r_vals <- loc_vals[seq(2,n_loc,2)]
    r_mat <- matrix(rep(r_vals, each = 16), ncol = 4, byrow = T)
    
    # Locations of axes labels and legend
    yl_mat <- matrix(n_bs + 1, nrow = nrow(l_mat))
    yr_mat <- matrix(n_bs + 2, nrow = nrow(l_mat))
    b_mat <- matrix(n_bs + 3, ncol = 10, nrow = 2)
    
    # Create layout matrix
    lay_mat <- rbind(cbind(yl_mat, l_mat, r_mat, yr_mat), b_mat)
  } else {
    l_vals <- 1
    r_vals <- 1
    lay_mat <- rbind(cbind(rep(2,4), matrix(1, ncol = 4, nrow = 4), rep(3,4)), rep(4,6), rep(4,6))
  }

  layout(lay_mat)
  par(mar = c(1,1,2,1))
  for (i in 1:n_bs) {
    # Pull data for selected sample
    sp_temp <- spline_df[spline_df$bs_index == bs_ids[i],]
    pod_temp <- pod_df[pod_df$bs_index == bs_ids[i],]
    plot_title <- paste0("Bootstrap Sample ", bs_ids[i])

    # Draw spline fit line
    plot(sp_temp[,scale_opts$dose_colname],
         sp_temp$response_pred,
         type        = "l",
         col         = "blue",
         lwd         = 2,
         panel.first = grid(),
         xlim        = range(x_ticks[,scale_opts$dose_colname]),
         ylim        = y_ax_lims,
         main        = plot_title,
         cex.main    = 1.5,
         font.main   = 1,
         xlab        = "",
         ylab        = "",
         xaxt        = "n",
         yaxt        = "n")

    # Add axes tick values
    if (i %in% c(n_bs-1, n_bs)) my_ax(scale_opts, x_ticks, draw_box = T, cex.axis = 1.5)
    if (i %in% l_vals) axis(side = 2, las = 2, cex.axis = 1.5)

    # Draw Menger Curvature
    par(new = T)
    plot(sp_temp[,scale_opts$dose_colname],
         sp_temp$mc,
         type = "l",
         col  = "goldenrod1",
         lwd  = 2,
         xaxt = "n",
         yaxt = "n",
         xlim = range(x_ticks[,scale_opts$dose_colname]),
         ylim = y_mc_lims,
         xlab = "",
         ylab = "")

    # Add axes tick values
    if (i %in% r_vals) axis(side = 4, las = 2, cex.axis = 1.5)

    # Add point at POD
    points(x = pod_temp[,scale_opts$dose_colname], y = pod_temp$mc, pch = 20)
  }

  # Add axis labels
  par(mar = c(1.1, 1.1, 1.1, 1.1))
  blank_plot(x = 0.9)
  text(0,0, "Predicted Response", srt = 90, cex = 1.5)
  
  blank_plot(x = -0.9)
  text(0,0, "Menger Curvature", srt = 270, cex = 1.5)
  
  blank_plot()
  text(0,0.5, scale_opts$dose_lab, cex = 1.5)
  legend("center",
         legend = c("Spline Fit", "Menger Curvature", "POD"),
         lwd    = 2,
         lty    = c("solid", "solid", NA),
         pch    = c(NA, NA, 20),
         col    = c("blue", "goldenrod1", "black"),
         xpd    = T,
         horiz  = T,
         bty    = "n",
         cex    = 1.5)

  par(op_def); layout(1)
}

# Draw data and spline fits for selected bootstraps
plot_bs_summary <- function(df, bs_ids, type = c("bs", "spline"), scale_opts, x_ticks) {
  n_bs <- length(unique(df$bs_index))
  df <- df[df$bs_index %in% bs_ids,]

  # Set plot labels
  if (type == "bs") {
    resp_colname <- "response"
    plot_title <- paste0("Bootstrap Sample Plot (",
                         length(bs_ids), " of ", n_bs, " samples)")
    resp_lab <- "Response"
  } else if (type == "spline") {
    resp_colname <- "response_pred"
    plot_title <- paste0("Plot of Interpolated Spline Predictions (",
                         length(bs_ids), " of ", n_bs, " samples)")
    resp_lab <- "Predicted Response"
  }

  plot(df[,scale_opts$dose_colname],
       df[,resp_colname],
       panel.first = grid(),
       pch  = 20,
       col  = rgb(0,0,0,0.25),
       xlim = range(x_ticks[,scale_opts$dose_colname]),
       xlab = scale_opts$dose_lab,
       ylab = resp_lab,
       main = plot_title,
       xaxt = "n",
       yaxt = "n",
       frame.plot = F)
  my_ax(scale_opts, x_ticks)
  axis(2, las = 2)
  for (i in bs_ids) {
    tmp <- df[df$bs_index == i,]
    lines(tmp[,scale_opts$dose_colname],
          tmp$response,
          col = rgb(0,0,0,0.25))
  }
}