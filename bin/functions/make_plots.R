if (!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
if (!require("gridExtra")) install.packages("gridExtra"); library("gridExtra")

my_theme <- theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_line(),
        panel.grid.minor = element_line())

plot_input_data <- function(in_list){
  df <- do.call("rbind", in_list)
  ggplot(df, aes(x = dose, y = response)) + 
    geom_point() + 
    labs(x = "Dose",
         y = "Response",
         title = "Input Data Plot") + 
    my_theme
}

plot_bs <- function(bs_df, n = 50) {
  bs_ids <- unique(bs_df$bs_index)
  if (length(bs_ids) > n){
    bs_ids <- sample(bs_ids, n)
    bs_df <- bs_df[bs_df$bs_index %in% bs_ids,]
  }
  
  ggplot(bs_df, aes(x = dose, y = response, group = as.factor(bs_index))) + 
    geom_point(alpha = 0.25) + 
    geom_path(alpha = 0.25) + 
    labs(x = "Dose",
         y = "Response",
         title = "Bootstrap Sample Plot") + 
    my_theme
}

plot_splines <- function(spline_df, n = 50){
  bs_ids <- unique(spline_df$bs_index)
  if (length(bs_ids) > n) {
    bs_ids <- sample(bs_ids, n)
    spline_df <- spline_df[spline_df$bs_index %in% bs_ids,]
  }

  ggplot(spline_df, aes(x = dose, y = response_pred, group = bs_index)) + 
    geom_point(alpha = 0.25) + 
    geom_path(alpha = 0.25) + 
    labs(x = "Dose",
         y = "Predicted Response",
         title = "Spline Predictions") + 
    my_theme
}

plot_pod_dist <- function(pod_df, pod_qs) {
  pod_qs <- data.frame(q = names(pod_qs), pod = pod_qs)
  pod_qs$q <- paste0(pod_qs$q, " = ", round(pod_qs$pod, 4))
  ggplot(pod_df, aes(x = dose)) + 
    geom_density(fill = "goldenrod1", alpha = 0.5) + 
    geom_vline(data = pod_qs, aes(xintercept = pod, linetype = q), color = "blue", alpha = 0.5, lwd = 1) +
    labs(x = "POD Estimates",
         title = "Distribution of POD Estimates") + 
    scale_linetype_discrete(name = "Quantiles") + 
    my_theme
}

plot_mc <- function(spline_df, mc_df, pods, bs_id){
  # Get data from bootstrap samples
  spline_df <- spline_df[spline_df$bs_index %in% bs_id,]
  mc_df <- mc_df[mc_df$bs_index %in% bs_id,]
  pods <- pods[pods$bs_index %in% bs_id,]
  
  # Scale y axis for MC
  reducer <- max(spline_df$response_pred)/max(mc_df$mc)
  
  # Create facet labels
  spline_df$bs_index <- paste0("Bootstrap Sample ", spline_df$bs_index)
  mc_df$bs_index <- paste0("Bootstrap Sample ", mc_df$bs_index)
  pods$bs_index <- paste0("Bootstrap Sample ", pods$bs_index)
  
  ggplot(spline_df, aes(x = dose, y = response_pred)) + 
    geom_line(aes(color = "spline_line")) + 
    geom_line(data = mc_df, aes(color = "mc_line", x = dose, y = mc)) + 
    geom_point(data = pods, aes(x = dose, y = mc, color = "pod_pt")) + 
    scale_y_continuous(sec.axis = sec_axis(~.*reducer, name = "Relative Curvature")) +
    labs(x = "Dose",
         y = "Predicted Response"
         ) + 
    scale_color_manual(name = "Legend",
                       values = c("blue", "goldenrod1", "black"),
                       limits = c("spline_line", "mc_line", "pod_pt"),
                       labels = c("Spline Curve", "Menger Curvature", "POD")) +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
    facet_wrap(.~bs_index)
}

plot_mc_summary <- function(spline_df, pod_df) {
  spline_df <- do.call("rbind", by(spline_df, spline_df$dose, function(dat) {
    data.frame(dose = dat$dose[1],min_resp = min(dat$response_pred), med_resp = median(dat$response_pred), max_resp = max(dat$response_pred))
  }))
  rownames(spline_df) <- NULL
  x_limits <- c(min(spline_df$dose) - 0.25, max(spline_df$dose) + 0.25)
  spline_plot <- ggplot(spline_df, aes(x = dose, y = med_resp, ymin = min_resp, ymax = max_resp)) + 
    geom_ribbon(alpha = 0.5, fill = "goldenrod1") + 
    geom_point(aes(color = "med")) + 
    geom_line() + 
    labs(x = "Dose",
         y = "Predicted Response",
         title = "Spline Fit Summary") + 
    scale_color_manual(name = "", values = "black", limits = "med", labels = "Median Response") + 
    my_theme + 
    xlim(x_limits) + 
    theme(legend.position = "top")
  pod_plot <- ggplot(pod_df, aes(x = dose)) + 
    geom_histogram(bins = length(unique(spline_df$dose))) + 
    labs(x = "POD",
         y = "Frequency",
         title = "POD Distribution") + 
    xlim(x_limits) + 
    my_theme
  grid.arrange(spline_plot, pod_plot, layout_matrix = matrix(c(1,1,1,2)))
}
