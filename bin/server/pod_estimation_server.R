################################
# Server for POD Estimation 
################################

### Analysis ---- 
# User clicks Run Analysis
res <- eventReactive(input$run_analysis, {
  withProgress(calculate_pod_quantiles(dr_dat(), resample_size = input$resample_size), message = "Calculating...")
})

# Display result table upon action button click
pod_res_table <- eventReactive(input$run_analysis, {
  datatable(res()$pods,
            colnames = c("Bootstrap Index", "POD (Original Scale)", "POD (Log\u2081\u2080 Scale)", "Menger Curvature"),
            rownames = FALSE,
            options = list(dom = "tlp"))
})

# Display results table
output$table <- renderDataTable({
  formatRound(pod_res_table(), columns = c(2,3,4), digits = 2)
})

# Draw plot of pod distribution
pod_dist_dose <- eventReactive(input$run_analysis,
                               plot_pod_dist(res()$pods, res()$pod_quantile, dr_dat(), "Original Doses"))

pod_dist_log <- eventReactive(input$run_analysis,
                               plot_pod_dist(res()$pods, res()$pod_quantile, dr_dat(), "Log10(Doses)"))

observeEvent(input$viewopt_pod_estimate, {
  if (input$viewopt_pod_estimate == "Original Doses") {
    output$pod_dist <- renderPlot(pod_dist_dose())
  } else if (input$viewopt_pod_estimate == "Log10(Doses)") {
    output$pod_dist <- renderPlot(pod_dist_log())
  }
})

# Draw bootstrap summary plot 
bs_summary_dose <- eventReactive(input$run_analysis,
                                 plot_mc_summary(res()$spline_predictions, res()$pods, dr_dat(), "Original Doses"))
bs_summary_log <- eventReactive(input$run_analysis,
                                 plot_mc_summary(res()$spline_predictions, res()$pods, dr_dat(), "Log10(Doses)"))

observeEvent(input$viewopt_pod_estimate, {
  if (input$viewopt_pod_estimate == "Original Doses") {
    output$bs_summary_plot <- renderPlot(bs_summary_dose())
  } else if (input$viewopt_pod_estimate == "Log10(Doses)") {
    output$bs_summary_plot <- renderPlot(bs_summary_log())
  }
})

# Save results 
output$downloadRes <- downloadHandler(
  filename = function() {
    paste0(format(Sys.time(), "%Y%m%d_%H%M_toxpodoutput.zip"))
  },
  content = function(file){
    dir   <- getwd()
    setwd(tempdir()); on.exit(setwd(dir))
    files <- c("input_data.txt", "analysis_parameters.txt", "pod_results.txt", "mc_results.txt", "pod_dist.png", "pod_dist_log.png", "bs_summary.png", "bs_summary_log.png")
    # Input data
    in_dat <- data.frame(do.call("rbind",dr_dat()), row.names = NULL)
    write.table(in_dat, files[1], row.names = F, sep = "\t", quote = FALSE)
    # Analysis summary 
    params <- c()
    if (input$data_source == "User Data"){
      if(input$user_input_choice == "Upload"){
        params <- c(params, 
                    "Data Source" = "User Upload",
                    "File Path" = input$user_fpath$name)
      } else if (input$user_input_choice == "Paste"){
        params <- c(params, "Data Source" = "User Pasted")
      }
    } else if (input$data_source == "Example") {
      params <- c(params,
                  "Data Source" = "Example",
                  "Example File" = input$example_choice)
    }
    params <- c(params, "Number of Bootstrap Samples" = input$resample_size)
    params <- data.frame(Parameter = names(params),
                         Value = params,
                         row.names = NULL)
    write.table(params, files[2], row.names = F, sep = "\t", quote = FALSE)
    # pod results
    pod_res <- res()$pods
    colnames(pod_res) <- c("bs_index", "pod", "log10_pod", "mc")
    write.table(pod_res, files[3], row.names = F, sep = "\t", quote = FALSE)
    # spline + mc results
    mc_res <- res()$menger_curvature
    spline_res <- res()$spline_predictions
    spline_res$mc <- mc_res$mc[match(interaction(spline_res$bs_index, spline_res$dose), interaction(mc_res$bs_index, mc_res$dose))]
    write.table(spline_res, files[4], row.names = F, sep = "\t", quote = FALSE)
    # pod distribution
    ggplot2::ggsave(files[5], pod_dist_dose(), width = 8, height = 4, unit = "in")
    ggplot2::ggsave(files[6], pod_dist_log(), width = 8, height = 4, unit = "in")
    # bootstrap summary
    ggplot2::ggsave(files[7], bs_summary_dose(), width = 8, height = 6, unit = "in")
    ggplot2::ggsave(files[8], bs_summary_log(), width = 8, height = 6, unit = "in")
    zip(file, files)
  },
  contentType = "application/zip"
)

