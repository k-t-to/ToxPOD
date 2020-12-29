# dev.off()
op_def <- par()
op_def <- op_def[!names(op_def) %in% c("cin", "cra", "csi", "cxy", "din", "page")]

# Input Data ----- 

plot_input_data <- function(in_list, dose_opt){
  df <- do.call("rbind", in_list)
  if(dose_opt == "Original Doses"){
    df <- df[,c("dose", "response")]
    x_ax_lab <- "Dose"
  }else if (dose_opt == "Log10(Doses)"){
    df <- df[,c("log10_dose", "response")]
    colnames(df) <- c("dose", "response")
    x_ax_lab <- expression(Log[10](Dose + 1))
  }
  plot(x = df$dose,
       y = df$response,
       panel.first = grid(),
       xlab = x_ax_lab,
       ylab = "Response",
       main = "Input Data Type",
       pch = 16,
       frame.plot = F,
       yaxt = "n")
  box(lwd = 2, bty = "l")
  axis(side = 2, las = 2)
}

# Analysis ----- 
# Distribution of PODs
plot_pod_dist <- function(pod_df, in_dat, viewopt, pod_qs = c(0.05, 0.95), op = op_def, median_line = T, mean_line = F) {
  pod_qs <- sort(pod_qs)
  if (viewopt == "Original Doses") {
    x_ax_lab <- "POD Estimates (Original Scale)"
    x_ax_lims <- range(sapply(in_dat, function(x) x$dose[1]))
  } else if(viewopt == "Log10(Doses)"){
    x_ax_lab <- expression(POD~Estimates~(Log[10]~Scale))
    x_ax_lims <- range(sapply(in_dat, function(x) x$log10_dose[1]))
    pod_df <- pod_df[,c("bs_index", "log10_dose")]
    colnames(pod_df)[2] <- "dose"
  }
  
  pod_q_l <- quantile(pod_df$dose, pod_qs[1])
  pod_q_u <- quantile(pod_df$dose, pod_qs[2])
  d <- density(pod_df$dose)
  ql_id <- min(which(d$x >= pod_q_l))
  qu_id <- max(which(d$x < pod_q_u))
  ql_y1 <- d$y[ql_id]
  qu_y1 <- d$y[qu_id]
  dd <- approxfun(d)
  legend_labs <- paste0(pod_qs * 100, "% = ", c(round(pod_q_l, 2), round(pod_q_u, 2)))
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
       frame.plot = F)
  box(lwd = 2, bty = "l")
  polygon(d, col="gray")
  segments(x0 = pod_q_l, y0 = 0, x1 = pod_q_l, y1 = ql_y1, lwd = 3, col = "blue")
  segments(x0 = pod_q_u, y0 = 0, x1 = pod_q_u, y1 = qu_y1, lwd = 3, col = "blue")
  with(d, polygon(x = c(x[c(ql_id, ql_id:qu_id, qu_id)]), y = c(0, y[ql_id: qu_id], 0), density = 20, border = NA))
  if (median_line) {
    med <- median(pod_df$dose)
    med_y <- dd(med)
    segments(x0 = med, y0 = 0, x1 = med, y1 = med_y, lwd = 3, col = "orange")
    legend_labs_d <- c(legend_labs_d, paste0("Median = ", round(med, 2)))
    legend_lines_d <- c(legend_lines_d, "solid")
    legend_cols_d <- c(legend_cols_d, "orange")
  }
  if (mean_line) {
    avg <- mean(pod_df$dose)
    avg_y <- dd(avg)
    segments(x0 = avg, y0 = 0, x1 = avg, y1 = avg_y, lwd = 3, col = "orange", lty = "dotted")
    legend_labs_d <- c(legend_labs_d, paste0("Mean = ", round(avg, 2)))
    legend_lines_d <- c(legend_lines_d, "dotted")
    legend_cols_d <- c(legend_cols_d, "orange")
  }
  
  par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), mar=c(4.1, 0, 4.1, 0), new=TRUE)
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n', xlab = "", ylab = "")
  legend("topright", legend = legend_labs, col = legend_cols, lty = legend_lines, title = "Quantiles", lwd = 3, cex = 0.75)
  if (median_line | mean_line) legend("right", legend = legend_labs_d, col = legend_cols_d, lty = legend_lines_d, lwd = 3, cex = 0.75)
  par(op)
}
# Bootstrap summary: splines + histogram 
plot_mc_summary <- function(spline_df, pod_df, in_dat, viewopt, op = op_def) {
  if (viewopt == "Original Doses") {
    x_ax_line_lab <- "Dose"
    x_ax_hist_lab <- "POD (Original Scale)"
    x_ax_lims <- range(sapply(in_dat, function(x) x$dose[1]))
    
  } else if(viewopt == "Log10(Doses)"){
    x_ax_line_lab <- expression(Log[10](Dose))
    x_ax_hist_lab <- expression(POD~(Log[10]~Scale))
    x_ax_lims <- range(sapply(in_dat, function(x) x$log10_dose[1]))
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
  
  layout(matrix(c(1,1,1,2), ncol = 1))
  
  plot(spline_df$dose, 
       spline_df$med_resp,
       panel.first = ribbon("minmax"),
       xlab = x_ax_line_lab,
       ylab = "Predicted Response",
       main = "Spline Fit Summary",
       pch = 20,
       type = "b",
       frame.plot = F,
       xlim = x_ax_lims)
  box(lwd = 2, bty = "l")
  legend("bottomright", legend = "Median Response", col = "black", pch = 16, lty = "solid", cex = 0.75, bg = rgb(1,1,1,0.5))
  par(mar = c(5, 4, 0, 2) + 0.1)
  hist(pod_df$dose, xlim = x_ax_lims, col = "gray", breaks = unique(spline_df$dose), main = "", xlab = x_ax_hist_lab)
  box(lwd = 2, bty = "l")
  par(op); layout(1)
}


# Sample Explorer ----- 
plot_mc <- function(spline_df, mc_df, pods, bs_ids, in_dat, viewopt, op = op_def) {
  if (viewopt == "Original Doses") {
    x_ax_lab <- "Dose"
    x_ax_lims <- range(sapply(in_dat, function(x) x$dose[1]))
    
  } else if(viewopt == "Log10(Doses)"){
    x_ax_lab <- expression(Log[10](Dose + 1))
    x_ax_lims <- range(sapply(in_dat, function(x) x$log10_dose[1]))
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
  n_c <- 2
  n_r <- ceiling(n_bs/n_c)
  lay_mat <- matrix(1:(n_c * n_r), ncol = n_c, byrow = T)
  
  # Set up logicals for drawing axis labels
  y_resp_ax <- lay_mat[,1]
  x_ax <- sort(lay_mat[lay_mat <= n_bs], decreasing = T)[1:n_c]
  tmp <- lay_mat
  tmp[tmp > n_bs] <- NA
  y_mc_ax <- apply(tmp, 1, max, na.rm = T)
  
  layout(lay_mat)
  par(mar = c(1,1,1,1), oma = c(8,5,2,5))
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
    if (i %in% x_ax) axis(side = 1)
    if (i %in% y_resp_ax) axis(side = 2, las = 2)
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
    if (i %in% y_mc_ax) axis(side = 4, las = 2)
    points(x = pod_temp$dose, y = pod_temp$mc, pch = 20)
  }
  mtext(x_ax_lab, side = 1, line = 2, outer = T, cex = 0.8)
  mtext("Predicted Response", side = 2, line = 2, outer = T, cex = 0.8)
  par(op)
  par(xpd = T, fig = c(0.95, 1, 0,1))
  text(0.5,0.5, "Menger Curvature", srt = 270, adj = 0.5, cex = 0.8)
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0.5, 0, 0, 0), new = TRUE)
  plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n', xlab = "", ylab = "")
  legend("bottom", legend = c("Spline Fit", "Menger Curvature", "POD"), lwd = 2, lty = c("solid", "solid", NA), pch = c(NA, NA, 20), col = c("blue", "goldenrod1", "black"), xpd = T, horiz = T, bty = "n")
  par(op)
}

plot_bs <- function(bs_df, bs_ids, viewopt) {
  n_bs <- length(unique(bs_df$bs_index))
  bs_df <- bs_df[bs_df$bs_index %in% bs_ids,]
  
  plot_title <- paste0("Bootstrap Sample Plot (", length(bs_ids), " of ", n_bs, " samples)")
  
  if (viewopt == "Original Doses") {
    x_ax_lab <- "Dose"
  } else if(viewopt == "Log10(Doses)"){
    bs_df <- bs_df[,c("bs_index", "log10_dose", "response")]
    colnames(bs_df)[2] <- "dose"
    x_ax_lab <- expression(Log[10](Dose + 1))
  }
  
  # Start plot
  plot(bs_df$dose,
       bs_df$response,
       panel.first = grid(),
       pch = 20,
       col = rgb(0,0,0,0.25),
       xlab = x_ax_lab,
       ylab = "Response",
       main = plot_title,
       yaxt = "n",
       frame.plot = F)
  box(lwd = 2, bty = "l")
  axis(2, las = 2)
  for (i in bs_ids) {
    tmp <- bs_df[bs_df$bs_index == i,]
    lines(tmp$dose,
          tmp$response,
          col = rgb(0,0,0,0.25))
  }
}

plot_splines <- function(spline_df, bs_ids, viewopt) {
  n_bs <- length(unique(spline_df$bs_index))
  spline_df <- spline_df[spline_df$bs_index %in% bs_ids,]
  
  plot_title <- paste0("Plot of Interpolated Spline Predictions (", length(bs_ids), " of ", n_bs, " samples)")
  
  if (viewopt == "Original Doses") {
    x_ax_lab <- "Dose"
  } else if(viewopt == "Log10(Doses)"){
    spline_df <- spline_df[,c("bs_index", "log10_dose", "response_pred")]
    colnames(spline_df)[2] <- "dose"
    x_ax_lab <- expression(Log[10](Dose + 1))
  }
  
  # Start plot
  plot(spline_df$dose,
       spline_df$response_pred,
       panel.first = grid(),
       pch = 20,
       col = rgb(0,0,0,0.25),
       xlab = x_ax_lab,
       ylab = "Predicted Response",
       main = plot_title,
       yaxt = "n",
       frame.plot = F)
  box(lwd = 2, bty = "l")
  axis(2, las = 2)
  for (i in bs_ids) {
    tmp <- spline_df[spline_df$bs_index == i,]
    lines(tmp$dose,
          tmp$response_pred,
          col = rgb(0,0,0,0.25))
  }
}