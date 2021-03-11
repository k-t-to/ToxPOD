# dev.off()
op_def <- par()
op_def <- op_def[!names(op_def) %in% c("cin", "cra", "csi", "cxy", "din", "page")]

# Input Data ----- 

plot_input_data <- function(in_list, dose_opt){
  df <- do.call("rbind", in_list)
  x_tick_labs <- signif(unique(df$dose), digits = 2)
  if(dose_opt == "Original Doses"){
    df <- df[,c("dose", "response")]
    x_ax_lab <- "Dose"
  }else if (dose_opt == "Log10(Doses)"){
    df <- df[,c("log10_dose", "response")]
    colnames(df) <- c("dose", "response")
    x_ax_lab <- expression("Dose ("*Log[10]~"Scale)")
  }
  plot(x = df$dose,
       y = df$response,
       panel.first = grid(),
       xlab = x_ax_lab,
       ylab = "Response",
       main = "Input Data Type",
       pch = 16,
       frame.plot = F,
       yaxt = "n",
       xaxt = "n")
  box(lwd = 2, bty = "l")
  axis(side = 2, las = 2)
  axis(side = 1, at = unique(df$dose), labels = x_tick_labs)
}

# Analysis ----- 
# Distribution of PODs
plot_pod_dist <- function(pod_df, in_dat, viewopt, pod_qs = c(0.025, 0.975), op = op_def, median_line, mean_line) {
  pod_qs <- sort(pod_qs)
  
  # Get results on original dose scale
  pods_og <- pod_df$dose
  pod_q_l <- quantile(pods_og, pod_qs[1])
  pod_q_u <- quantile(pods_og, pod_qs[2])
  
  # Set x-axis tick labels
  x_tick_labs <- sapply(in_dat, function(x) signif(x$dose[1], digits = 2))
  
  if (viewopt == "Original Doses") {
    x_ax_lab <- "POD Estimates (Original Scale)"
    x_ax_at <- sapply(in_dat, function(x) x$dose[1])
    x_ax_lims <- range(x_ax_at)
    # Set quantile line location 
    q_l_line <- pod_q_l
    q_u_line <- pod_q_u
    
  } else if(viewopt == "Log10(Doses)"){
    x_ax_lab <- expression(POD~Estimates~(Log[10]~Scale))
    x_ax_at <- sapply(in_dat, function(x) x$log10_dose[1])
    x_ax_lims <- range(x_ax_at)
    
    # Set quantile line location
    q_l_line <- quantile(pod_df$log10_dose, pod_qs[1])
    q_u_line <- quantile(pod_df$log10_dose, pod_qs[2])
    
    pod_df <- pod_df[,c("bs_index", "log10_dose")]
    colnames(pod_df)[2] <- "dose"
  }

  d <- density(pod_df$dose)
  ql_id <- min(which(d$x >= q_l_line))
  qu_id <- max(which(d$x < q_u_line))
  ql_y1 <- d$y[ql_id]
  qu_y1 <- d$y[qu_id]
  dd <- approxfun(d)
  legend_labs <- paste0(pod_qs * 100, "% = ", c(signif(pod_q_l, 2), signif(pod_q_u, 2)))
  legend_lines <- c("solid", "solid")
  legend_cols <- c("blue", "blue")
  
  legend_labs_d <- legend_lines_d <- legend_cols_d <- c()
  
  par(mar = c(5.1,4.1,4.1,8.1))
  plot(d,
       panel.first = grid(),
       xlab = x_ax_lab,
       ylab = "Density",
       xlim = x_ax_lims,
       main = "Distribution of POD Estimates",
       frame.plot = F,
       xaxt = "n")
  axis(side = 1, at = x_ax_at, labels = x_tick_labs)
  box(lwd = 2, bty = "l")

  polygon(d, col="gray55")
  segments(x0 = q_l_line, y0 = 0, x1 = q_l_line, y1 = ql_y1, lwd = 3, col = "blue")
  segments(x0 = q_u_line, y0 = 0, x1 = q_u_line, y1 = qu_y1, lwd = 3, col = "blue")
  with(d, polygon(x = c(x[c(ql_id, ql_id:qu_id, qu_id)]), y = c(0, y[ql_id: qu_id], 0), col = "gray25", border = NA))
  if (median_line) {
    # Median on original scale
    med_og <- median(pods_og) # For legend
    # Median for plot
    med_plot <- median(pod_df$dose)
    med_y <- dd(med_plot)
    segments(x0 = med_plot, y0 = 0, x1 = med_plot, y1 = med_y, lwd = 3, col = "orange")
    legend_labs_d <- c(legend_labs_d, paste0("Median = ", signif(med_og, 2)))
    legend_lines_d <- c(legend_lines_d, "solid")
    legend_cols_d <- c(legend_cols_d, "orange")
  }
  if (mean_line) {
    # Mean on original scale
    avg_og <- mean(pods_og)
    # Mean for plot
    avg_plot <- mean(pod_df$dose)
    avg_y <- dd(avg_plot)
    segments(x0 = avg_plot, y0 = 0, x1 = avg_plot, y1 = avg_y, lwd = 3, col = "orange", lty = "dotted")
    legend_labs_d <- c(legend_labs_d, paste0("Mean = ", signif(avg_og, 2)))
    legend_lines_d <- c(legend_lines_d, "dotted")
    legend_cols_d <- c(legend_cols_d, "orange")
  }
  
  par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), mar=c(4.1, 0, 4.1, 0), new=TRUE)
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n', xlab = "", ylab = "")
  legend("topright", legend = legend_labs, col = legend_cols, lty = legend_lines, title = "POD Credible Interval", lwd = 3, cex = 0.75)
  if (median_line | mean_line) legend("right", legend = legend_labs_d, col = legend_cols_d, lty = legend_lines_d, lwd = 3, cex = 0.75)

  par(op)
}
# Bootstrap summary: splines + histogram 
plot_mc_summary <- function(spline_df, pod_df, in_dat, viewopt, pod_qs = c(0.025, 0.975),  op = op_def, median_line, mean_line) {
  
  # Get results on original dose scale
  pods_og <- pod_df$dose
  pod_q_l <- quantile(pods_og, pod_qs[1])
  pod_q_u <- quantile(pods_og, pod_qs[2])
  
  # Set x-axis tick labels
  x_tick_labs <- sapply(in_dat, function(x) signif(x$dose[1], digits = 2))
  
  if (viewopt == "Original Doses") {
    x_ax_line_lab <- "Dose"
    x_ax_hist_lab <- "POD (Original Scale)"
    x_ax_at <- sapply(in_dat, function(x) x$dose[1])
    x_ax_lims <- range(x_ax_at)
    
    # Set quantile line location 
    q_l_line <- pod_q_l
    q_u_line <- pod_q_u
    
  } else if(viewopt == "Log10(Doses)"){
    x_ax_line_lab <- expression(Dose~(Log[10]~Scale))
    x_ax_hist_lab <- expression(POD~Estimates~(Log[10]~Scale))
    x_ax_at <- sapply(in_dat, function(x) x$log10_dose[1])
    x_ax_lims <- range(x_ax_at)
    
    # Set quantile line location
    q_l_line <- quantile(pod_df$log10_dose, pod_qs[1])
    q_u_line <- quantile(pod_df$log10_dose, pod_qs[2])
    
    spline_df <- spline_df[,c("bs_index", "log10_dose", "response_pred")]
    pod_df <- pod_df[,c("bs_index", "log10_dose", "mc")]
    colnames(spline_df)[2] <- "dose"
    colnames(pod_df)[2] <- "dose"
  }
  
  spline_df <- do.call("rbind", by(spline_df, spline_df$dose, function(dat) {
    data.frame(dose = dat$dose[1],
               min_resp = min(dat$response_pred), 
               med_resp = median(dat$response_pred), 
               max_resp = max(dat$response_pred),
               q05_resp = quantile(dat$response_pred, 0.05),
               q95_resp = quantile(dat$response_pred, 0.95))
  }))
  ribbon <- function(type = c("minmax", "quant")) {
    grid()
    if (type == "minmax") {
      polygon(x = c(spline_df$dose, rev(spline_df$dose)), y = c(spline_df$min_resp, rev(spline_df$max_resp)), col=rgb(1,0.75,0.15,0.5), border = F)
    } else if (type == "quant") {
      polygon(x = c(spline_df$dose, rev(spline_df$dose)), y = c(spline_df$q05_resp, rev(spline_df$q95_resp)), col=rgb(1,0.75,0.15,0.5), border = F)
    }
  }
  
  legend_labs <- paste0(pod_qs * 100, "% = ", c(signif(pod_q_l, 2), signif(pod_q_u, 2)))
  legend_lines <- c("solid", "solid")
  legend_cols <- c("blue", "blue")
  
  legend_labs_d <- legend_lines_d <- legend_cols_d <- c()
  
  layout(cbind(matrix(rep(c(1,1,1,2), 4), nrow = 4), c(3,3,3,3)))
  # layout(matrix(c(1,1,1,2), ncol = 1))
  
  plot(spline_df$dose, 
       spline_df$med_resp,
       panel.first = ribbon("minmax"),
       xlab = x_ax_line_lab,
       ylab = "Predicted Response",
       main = "Spline Fit Summary",
       pch = 20,
       type = "b",
       frame.plot = F,
       xlim = x_ax_lims,
       xaxt = "n")
  axis(side = 1, at = x_ax_at, labels = x_tick_labs)
  box(lwd = 2, bty = "l")
  
  par(mar = c(5, 4, 0, 4) + 0.1)
  hist(pod_df$dose, 
       xlim = x_ax_lims, 
       col = "gray", 
       breaks = unique(spline_df$dose), 
       main = "", 
       xlab = x_ax_hist_lab,
       xaxt = "n")
  axis(side = 1, at = x_ax_at, labels = x_tick_labs)
  box(lwd = 2, bty = "l")
  abline(v = c(q_l_line, q_u_line), lwd = 2, col = "blue")
  if (median_line) {
    # Median on Original Scale
    med_og <- median(pods_og)
    # Median for plot
    med_plot <- median(pod_df$dose)
    abline(v = med_plot, lwd = 2, col = "orange")
    legend_labs_d <- c(legend_labs_d, paste0("Median = ", signif(med_og, 2)))
    legend_lines_d <- c(legend_lines_d, "solid")
    legend_cols_d <- c(legend_cols_d, "orange")
  }
  
  if (mean_line) {
    # Median on Original Scale
    avg_og <- mean(pods_og)
    # Median for plot
    avg_plot <- mean(pod_df$dose)
    abline(v = avg_plot, lwd = 2, col = "orange", lty = "dotted")
    legend_labs_d <- c(legend_labs_d, paste0("Mean = ", signif(avg_og, 2)))
    legend_lines_d <- c(legend_lines_d, "dotted")
    legend_cols_d <- c(legend_cols_d, "orange")
  }
  
  par(mar = c(5.1,0,4.1,0))
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n', xlab = "", ylab = "", xlim = c(-1,1), ylim = c(-1,1))
  legend(-1,0.35, 
         legend = c("Median Response", "(Min, Max)"), 
         col = c("black", rgb(1,0.75,0.15,0.5)), 
         pch = c(16, NA), 
         fill = c(NA, rgb(1,0.75,0.15,1)),
         border = NA,
         lty = c("solid", NA))
  
  # plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n', xlab = "", ylab = "")
  legend(-1,-0.55, legend = legend_labs, col = legend_cols, lty = legend_lines, title = "POD Credible Interval", lwd = 3)
  if (median_line | mean_line) legend(-1, -0.85, legend = legend_labs_d, col = legend_cols_d, lty = legend_lines_d, lwd = 3)
  par(op); layout(1)
}


# Sample Explorer ----- 
plot_mc <- function(spline_df, mc_df, pods, bs_ids, in_dat, viewopt, op = op_def) {
  x_tick_labs <- sapply(in_dat, function(x) signif(x$dose[1], digits = 2))
  
  if (viewopt == "Original Doses") {
    x_ax_lab <- "Dose"
    x_ax_at <- sapply(in_dat, function(x) x$dose[1])
    x_ax_lims <- range(x_ax_at)
    
  } else if(viewopt == "Log10(Doses)"){
    x_ax_lab <- expression(Dose~(Log[10]~Scale))
    x_ax_at <- sapply(in_dat, function(x) x$log10_dose[1])
    x_ax_lims <- range(x_ax_at)
    spline_df <- spline_df[,c("bs_index", "log10_dose", "response_pred")]
    mc_df <- mc_df[,c("bs_index", "log10_dose", "mc")]
    pods <- pods[,c("bs_index", "log10_dose", "mc")]
    colnames(spline_df)[2] <- "dose"
    colnames(mc_df)[2] <- "dose"
    colnames(pods)[2] <- "dose"
  }
  
  # Get y limits
  y_ax_lims <- c(min(spline_df$response_pred) - 0.1, max(spline_df$response_pred) + 0.1)
  y_mc_lims <- c(min(mc_df$mc) - 0.1, max(mc_df$mc) + 0.1)

  # Create plotting layout
  n_bs <- length(bs_ids)
  if(n_bs > 1){
    if (n_bs %% 2 == 0) {
      loc_vals <- 1:n_bs
      n_loc <- n_bs
    } else {
      loc_vals <- c(1:n_bs, (n_bs+4))
      n_loc <- n_bs + 1
    }
    
    l_vals <- loc_vals[seq(1,n_loc,2)]
    r_vals <- loc_vals[seq(2,n_loc,2)]
    
    l_mat <- matrix(rep(l_vals, each = 16), ncol = 4, byrow = T)
    r_mat <- matrix(rep(r_vals, each = 16), ncol = 4, byrow = T)
    yl_mat <- matrix(n_bs + 1, nrow = nrow(l_mat))
    yr_mat <- matrix(n_bs + 2, nrow = nrow(l_mat))
    b_mat <- matrix(n_bs + 3, ncol = 10, nrow = 2)
    lay_mat <- rbind(cbind(yl_mat, l_mat, r_mat, yr_mat), b_mat)
  } else {
    l_vals <- 1
    r_vals <- 1
    
    lay_mat <- rbind(cbind(rep(2,4), matrix(1, ncol = 4, nrow = 4), rep(3,4)), rep(4,6), rep(4,6))
  }

  layout(lay_mat)
  
  par(mar = c(1,1,1,1))
  for (i in 1:n_bs) {
    sp_temp <- spline_df[spline_df$bs_index == bs_ids[i],]
    mc_temp <- mc_df[mc_df$bs_index == bs_ids[i],]
    pod_temp <- pods[pods$bs_index == bs_ids[i],]
    plot_title <- paste0("Bootstrap Sample ", bs_ids[i])
    
    plot(sp_temp$dose,
         sp_temp$response_pred,
         type = "l",
         col = "blue",
         lwd = 2,
         panel.first = grid(),
         xlim = x_ax_lims,
         ylim = y_ax_lims,
         xlab = "",
         ylab = "",
         xaxt = "n",
         yaxt = "n")
    title(main = plot_title, cex.main = 1, font.main = 1)
    if (i %in% c(n_bs-1, n_bs)) axis(side = 1, at = x_ax_at, labels = x_tick_labs)
    if (i %in% l_vals) axis(side = 2, las = 2)
    par(new = T)
    plot(mc_temp$dose,
         mc_temp$mc,
         type = "l",
         col = "goldenrod1",
         lwd = 2,
         xaxt = "n",
         yaxt = "n",
         xlim = x_ax_lims,
         ylim = y_mc_lims,
         xlab = "",
         ylab = "")
    if (i %in% r_vals) axis(side = 4, las = 2)
    points(x = pod_temp$dose, y = pod_temp$mc, pch = 20)
  }
  
  par(mar = c(1.1, 1.1, 1.1, 1.1))
  plot(0.9, 0, type='n', bty='n', xaxt='n', yaxt='n', xlab = "", ylab = "", xlim = c(-1,1), ylim = c(-1,1))
  text(0,0, "Predicted Response", srt = 90)
  
  plot(-0.9, 0, type='n', bty='n', xaxt='n', yaxt='n', xlab = "", ylab = "", xlim = c(-1,1), ylim = c(-1,1))
  text(0,0, "Menger Curvature", srt = 270)
  
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n', xlab = "", ylab = "", xlim = c(-1,1), ylim = c(-1,1))
  text(0,0.5, x_ax_lab)
  legend("center", legend = c("Spline Fit", "Menger Curvature", "POD"), lwd = 2, lty = c("solid", "solid", NA), pch = c(NA, NA, 20), col = c("blue", "goldenrod1", "black"), xpd = T, horiz = T, bty = "n")

  par(op)
}

plot_bs <- function(bs_df, bs_ids, in_dat, viewopt) {
  n_bs <- length(unique(bs_df$bs_index))
  bs_df <- bs_df[bs_df$bs_index %in% bs_ids,]
  
  x_tick_labs <- sapply(in_dat, function(x) signif(x$dose[1], digits = 2))
  
  plot_title <- paste0("Bootstrap Sample Plot (", length(bs_ids), " of ", n_bs, " samples)")
  
  if (viewopt == "Original Doses") {
    x_ax_lab <- "Dose"
    x_ax_at <- sapply(in_dat, function(x) x$dose[1])
    x_ax_lims <- range(x_ax_at)
  } else if(viewopt == "Log10(Doses)"){
    x_ax_at <- sapply(in_dat, function(x) x$log10_dose[1])
    x_ax_lims <- range(x_ax_at)
    
    bs_df <- bs_df[,c("bs_index", "log10_dose", "response")]
    colnames(bs_df)[2] <- "dose"
    x_ax_lab <- expression(Dose~(Log[10]~Scale))
  }
  
  # Start plot
  plot(bs_df$dose,
       bs_df$response,
       panel.first = grid(),
       pch = 20,
       col = rgb(0,0,0,0.25),
       xlim = x_ax_lims,
       xlab = x_ax_lab,
       ylab = "Response",
       main = plot_title,
       xaxt = "n",
       yaxt = "n",
       frame.plot = F)
  box(lwd = 2, bty = "l")
  axis(2, las = 2)
  axis(1, at = x_ax_at, labels = x_tick_labs)
  for (i in bs_ids) {
    tmp <- bs_df[bs_df$bs_index == i,]
    lines(tmp$dose,
          tmp$response,
          col = rgb(0,0,0,0.25))
  }
}

plot_splines <- function(spline_df, bs_ids, in_dat, viewopt) {
  n_bs <- length(unique(spline_df$bs_index))
  spline_df <- spline_df[spline_df$bs_index %in% bs_ids,]
  
  plot_title <- paste0("Plot of Interpolated Spline Predictions (", length(bs_ids), " of ", n_bs, " samples)")
  
  x_tick_labs <- sapply(in_dat, function(x) signif(x$dose[1], digits = 2))
  
  if (viewopt == "Original Doses") {
    x_ax_lab <- "Dose"
    x_ax_at <- sapply(in_dat, function(x) x$dose[1])
    x_ax_lims <- range(x_ax_at)
  } else if(viewopt == "Log10(Doses)"){
    x_ax_at <- sapply(in_dat, function(x) x$log10_dose[1])
    x_ax_lims <- range(x_ax_at)
    
    spline_df <- spline_df[,c("bs_index", "log10_dose", "response_pred")]
    colnames(spline_df)[2] <- "dose"
    x_ax_lab <- expression(Dose~(Log[10]~Scale))
  }
  
  # Start plot
  plot(spline_df$dose,
       spline_df$response_pred,
       panel.first = grid(),
       pch = 20,
       col = rgb(0,0,0,0.25),
       xlim = x_ax_lims,
       xlab = x_ax_lab,
       ylab = "Predicted Response",
       main = plot_title,
       xaxt = "n",
       yaxt = "n",
       frame.plot = F)
  box(lwd = 2, bty = "l")
  axis(1, at = x_ax_at, labels = x_tick_labs)
  axis(2, las = 2)
  for (i in bs_ids) {
    tmp <- spline_df[spline_df$bs_index == i,]
    lines(tmp$dose,
          tmp$response_pred,
          col = rgb(0,0,0,0.25))
  }
}