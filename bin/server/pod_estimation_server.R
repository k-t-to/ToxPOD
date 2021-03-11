################################
# Server for POD Estimation 
################################

### Analysis ---- 
# User clicks Run Analysis
res <- eventReactive(input$run_analysis, {
  withProgress(calculate_pod_quantiles(dr_dat(), resample_size = input$resample_size), message = "Calculating...")
})

observeEvent(input$run_analysis|input$pod_ql, {
  dose_check <- sort(as.numeric(names(dr_dat())))[2]
  pod_check <- quantile(res()$pods$dose, input$pod_ql)
  if (pod_check < dose_check) {
    shinyalert(title = "WARNING",
               text = p(strong("POD Credible Interval violates low-dose asymptote assumption.\n"),
                        br(),
                        "POD estimation assumes that the input dose-response data are asymptotic 
                        at low-doses. The lower limit of the POD Credible Interval is within the 
                        two smallest doses. Verify that an asymptote has been established at 
                        lower doses of the input data."),
               type = "warning",
               html = TRUE,
               size = "m")
  }
})

# Display result table upon action button click
pod_res_table <- eventReactive(input$run_analysis, {
  datatable(res()$pods[,c("bs_index", "dose", "mc")],
            colnames = c("Bootstrap Index", "POD", "Menger Curvature"),
            rownames = FALSE,
            options = list(dom = "tlp"))
})

output$pod_dist <- renderPlot({plot_pod_dist(pod_df = res()$pods, 
                                             in_dat = dr_dat(), 
                                             viewopt = input$viewopt_pod_estimate,
                                             pod_qs = c(input$pod_ql, input$pod_qu),
                                             median_line = input$med_line,
                                             mean_line = input$mean_line)})

output$pod_table <- output$bs_table <- renderDataTable({
  formatRound(pod_res_table(), columns = c(2,3), digits = 2)
})

# Draw bootstrap summary plot 
output$bs_summary_plot <- renderPlot({plot_mc_summary(spline_df = res()$spline_predictions, 
                                                      pod_df = res()$pods, 
                                                      in_dat = dr_dat(), 
                                                      viewopt = input$viewopt_pod_estimate,
                                                      pod_qs = c(input$pod_ql, input$pod_qu),
                                                      median_line = input$med_line,
                                                      mean_line = input$mean_line)})


# Save results 
output$downloadRes <- downloadHandler(
  filename = function() {
    paste0(format(Sys.time(), "%Y%m%d_%H%M_toxpodoutput.zip"))
  },
  content = function(file){
    dir   <- getwd()
    setwd(tempdir()); on.exit(setwd(dir))
    files <- c("input_data.txt", "analysis_parameters.txt",
               "pod_results_intervals.txt", "pod_results_all.txt",
               "mc_results.txt", "pod_dist.pdf", "pod_dist_log.pdf", 
               "bs_summary.pdf", "bs_summary_log.pdf")
    
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
    # Quantile results
    quants <- sort(unique(c(input$pod_ql, input$pod_qu, 0.025, 0.975, 0.5)))
    quant_res <- quantile(res()$pods$dose, quants)
    quant_res <- data.frame(Quantile = names(quant_res), POD = quant_res)
    write.table(quant_res, files[3], row.names = F, sep = "\t", quote = FALSE)
    # pod results
    pod_res <- res()$pods
    colnames(pod_res) <- c("bs_index", "pod", "log10_pod", "mc")
    write.table(pod_res, files[4], row.names = F, sep = "\t", quote = FALSE)
    # spline + mc results
    mc_res <- res()$menger_curvature
    spline_res <- res()$spline_predictions
    spline_res$mc <- mc_res$mc[match(interaction(spline_res$bs_index, spline_res$dose), interaction(mc_res$bs_index, mc_res$dose))]
    write.table(spline_res, files[5], row.names = F, sep = "\t", quote = FALSE)
    
   # pod distribution
    pdf(files[6], width = 8, height = 4)
    plot_pod_dist(pod_df = res()$pods, 
                  in_dat = dr_dat(), 
                  viewopt = "Original Doses",
                  pod_qs = c(input$pod_ql, input$pod_qu),
                  median_line = input$med_line,
                  mean_line = input$mean_line)
    dev.off()
    pdf(files[7], width = 8, height = 4)
    plot_pod_dist(pod_df = res()$pods, 
                  in_dat = dr_dat(), 
                  viewopt = "Log10(Doses)",
                  pod_qs = c(input$pod_ql, input$pod_qu),
                  median_line = input$med_line,
                  mean_line = input$mean_line)
    dev.off()
    # bootstrap summary
    pdf(files[8], width = 8, height = 6)
    plot_mc_summary(spline_df = res()$spline_predictions, 
                    pod_df = res()$pods, 
                    in_dat = dr_dat(), 
                    viewopt = "Original Doses",
                    pod_qs = c(input$pod_ql, input$pod_qu),
                    median_line = input$med_line,
                    mean_line = input$mean_line)
    dev.off()
    pdf(files[9], width = 8, height = 6)
    plot_mc_summary(spline_df = res()$spline_predictions, 
                    pod_df = res()$pods, 
                    in_dat = dr_dat(), 
                    viewopt = "Log10(Doses)",
                    pod_qs = c(input$pod_ql, input$pod_qu),
                    median_line = input$med_line,
                    mean_line = input$mean_line)
    dev.off()
    
    zip(file, files)
  },
  contentType = "application/zip"
)

