if (!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
if (!require("gridExtra")) install.packages("gridExtra"); library("gridExtra")

my_theme <- theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_line(),
        panel.grid.minor = element_line())

# Input Data ----- 

plot_input_data <- function(in_list, dose_opt){
  df <- do.call("rbind", in_list)
  if(dose_opt == "Original Doses"){
    df <- df[,c("dose", "response")]
    x_ax_lab <- "Dose"
  }else if (dose_opt == "Log10(Doses)"){
    df <- df[,c("log10_dose", "response")]
    colnames(df) <- c("dose", "response")
    x_ax_lab <- expression(Log[10](Dose))
  }
  ggplot(df, aes(x = dose, y = response)) + 
    geom_point() + 
    labs(x = x_ax_lab,
         y = "Response",
         title = "Input Data Plot") + 
    my_theme
}

# Analysis ----- 
# Distribution of PODs
plot_pod_dist <- function(pod_df, pod_qs, in_dat, viewopt) {
  if (viewopt == "Original Doses") {
    x_ax_lab <- "POD Estimates (Original Scale)"
    x_ax_lims <- range(sapply(in_dat, function(x) x$dose[1]))
    pod_qs <- pod_qs$dose
  } else if(viewopt == "Log10(Doses)"){
    x_ax_lab <- expression(POD~Estimates~(Log[10]~Scale))
    x_ax_lims <- range(sapply(in_dat, function(x) x$log10_dose[1]))
    pod_qs <- pod_qs$log10_dose
    pod_df <- pod_df[,c("bs_index", "log10_dose")]
    colnames(pod_df)[2] <- "dose"
  }
  pod_qs <- data.frame(q = names(pod_qs), pod = pod_qs)
  
  pod_qs$q <- paste0(pod_qs$q, " = ", round(pod_qs$pod, 4))
  ggplot(pod_df, aes(x = dose)) + 
    geom_density(fill = "goldenrod1", alpha = 0.5) + 
    geom_vline(data = pod_qs, aes(xintercept = pod, linetype = q), color = "blue", alpha = 0.5, lwd = 1) +
    labs(x = x_ax_lab,
         title = "Distribution of POD Estimates") + 
    scale_linetype_discrete(name = "Quantiles") + 
    xlim(x_ax_lims) + 
    my_theme
}

# Bootstrap summary: splines + histogram 
plot_mc_summary <- function(spline_df, pod_df, in_dat, viewopt) {
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
    data.frame(dose = dat$dose[1],min_resp = min(dat$response_pred), med_resp = median(dat$response_pred), max_resp = max(dat$response_pred))
  }))
  rownames(spline_df) <- NULL
  
  spline_plot <- ggplot(spline_df, aes(x = dose, y = med_resp, ymin = min_resp, ymax = max_resp)) + 
    geom_ribbon(alpha = 0.5, fill = "goldenrod1") + 
    geom_point(aes(color = "med")) + 
    geom_line() + 
    labs(x = x_ax_line_lab,
         y = "Predicted Response",
         title = "Spline Fit Summary") + 
    scale_color_manual(name = "", values = "black", limits = "med", labels = "Median Response") + 
    my_theme + 
    xlim(x_ax_lims) + 
    theme(legend.position = "top")
  pod_plot <- ggplot(pod_df, aes(x = dose)) + 
    geom_histogram(breaks = unique(pod_df$dose)) + 
    labs(x = x_ax_hist_lab,
         y = "Frequency",
         title = "POD Distribution") + 
    xlim(x_ax_lims) + 
    my_theme
  grid.arrange(spline_plot, pod_plot, layout_matrix = matrix(c(1,1,1,2)))
}

# Sample Explorer ----- 
plot_mc <- function(spline_df, mc_df, pods, bs_id, in_dat, viewopt){
  if (viewopt == "Original Doses") {
    x_ax_lab <- "Dose"
    x_ax_lims <- range(sapply(in_dat, function(x) x$dose[1]))
    
  } else if(viewopt == "Log10(Doses)"){
    x_ax_lab <- expression(Log[10](Dose))
    x_ax_lims <- range(sapply(in_dat, function(x) x$log10_dose[1]))
    spline_df <- spline_df[,c("bs_index", "log10_dose", "response_pred")]
    mc_df <- mc_df[,c("bs_index", "log10_dose", "mc")]
    pods <- pods[,c("bs_index", "log10_dose", "mc")]
    colnames(spline_df)[2] <- "dose"
    colnames(mc_df)[2] <- "dose"
    colnames(pods)[2] <- "dose"
  }
  
  # Get data from bootstrap samples
  spline_df <- spline_df[spline_df$bs_index %in% bs_id,]
  mc_df <- mc_df[mc_df$bs_index %in% bs_id,]
  pods <- pods[pods$bs_index %in% bs_id,]
  
  # Scale y axis for MC
  reducer <- max(spline_df$response_pred)/max(mc_df$mc)
  
  # Create facet labels
  facet_levels <- sort(bs_id)
  facet_labels <- paste0("Bootstrap Sample ", facet_levels)
  spline_df$bs_index <- factor(spline_df$bs_index, levels = facet_levels, labels = facet_labels)
  mc_df$bs_index <- factor(mc_df$bs_index, levels = facet_levels, labels = facet_labels)
  pods$bs_index <- factor(pods$bs_index, levels = facet_levels, labels = facet_labels)
  
  ggplot(spline_df, aes(x = dose, y = response_pred)) + 
    geom_line(aes(color = "spline_line")) + 
    geom_line(data = mc_df, aes(color = "mc_line", x = dose, y = mc * reducer)) + 
    geom_point(data = pods, aes(x = dose, y = mc * reducer, color = "pod_pt")) + 
    scale_y_continuous(sec.axis = sec_axis(~./reducer, name = "Menger Curvature")) +
    labs(x = x_ax_lab,
         y = "Predicted Response"
    ) + 
    scale_color_manual(name = "Legend",
                       values = c("blue", "goldenrod1", "black"),
                       limits = c("spline_line", "mc_line", "pod_pt"),
                       labels = c("Spline Curve", "Menger Curvature", "POD")) +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
    xlim(x_ax_lims) + 
    facet_wrap(.~bs_index, ncol = 2)
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
    x_ax_lab <- expression(Log[10](Dose))
  }
  
  ggplot(bs_df, aes(x = dose, y = response, group = as.factor(bs_index))) + 
    geom_point(alpha = 0.25) + 
    geom_path(alpha = 0.25) + 
    labs(x = x_ax_lab,
         y = "Response",
         title = plot_title) + 
    my_theme
}

plot_splines <- function(spline_df, bs_ids, viewopt){
  n_bs <- length(unique(spline_df$bs_index))
  spline_df <- spline_df[spline_df$bs_index %in% bs_ids,]
  
  plot_title <- paste0("Plot of Interpolated Spline Predictions (", length(bs_ids), " of ", n_bs, " samples)")

  if (viewopt == "Original Doses") {
    x_ax_lab <- "Dose"
  } else if(viewopt == "Log10(Doses)"){
    spline_df <- spline_df[,c("bs_index", "log10_dose", "response_pred")]
    colnames(spline_df)[2] <- "dose"
    x_ax_lab <- expression(Log[10](Dose))
  }
  
  ggplot(spline_df, aes(x = dose, y = response_pred, group = bs_index)) + 
    geom_point(alpha = 0.25) + 
    geom_path(alpha = 0.25) + 
    labs(x = x_ax_lab,
         y = "Predicted Response",
         title = plot_title) + 
    my_theme
}



