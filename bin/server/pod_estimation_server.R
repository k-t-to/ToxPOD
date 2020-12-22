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

# Display results
output$pod_dist <- renderPlot({plot_pod_dist(res()$pods, res()$pod_quantile, dr_dat(), input$viewopt_pod_estimate)})

output$table <- renderDataTable({
  formatRound(pod_res_table(), columns = c(2,3,4), digits = 2)
})

# Draw bootstrap summary plot 
output$bs_summary_plot <- renderPlot({plot_mc_summary(res()$spline_predictions, res()$pods, dr_dat(), input$viewopt_pod_estimate)})


# Save results 
output$downloadRes <- downloadHandler(
  filename = function() {
    paste0(format(Sys.time(), "%Y%m%d_%H%M_toxpodoutput.zip"))
  },
  content = function(file){
    dir   <- getwd()
    setwd(tempdir()); on.exit(setwd(dir))
    files <- c("input_data.txt", "analysis_parameters.txt", "pod_results.txt", "mc_results.txt", "pod_dist.pdf", "pod_dist_log.pdf", "bs_summary.pdf", "bs_summary_log.pdf")
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
    pdf(files[5], width = 8, height = 4)
    plot_pod_dist(res()$pods, res()$pod_quantile, dr_dat(), "Original Doses")
    dev.off()
    pdf(files[6], width = 8, height = 4)
    plot_pod_dist(res()$pods, res()$pod_quantile, dr_dat(), "Log10(Doses)")
    dev.off()
    # bootstrap summary
    pdf(files[7])
    plot_mc_summary(res()$spline_predictions, res()$pods, dr_dat(), "Original Doses")
    dev.off()
    pdf(files[8])
    plot_mc_summary(res()$spline_predictions, res()$pods, dr_dat(), "Log10(Doses)")
    dev.off()
    
    zip(file, files)
  },
  contentType = "application/zip"
)

