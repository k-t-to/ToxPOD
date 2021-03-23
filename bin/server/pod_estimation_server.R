################################
# Server for POD Estimation 
################################

### Analysis ---- 
# User clicks Run Analysis
res <- eventReactive(input$run_analysis, {
  withProgress(calculate_pod_estimates(dr_dat(),
                                       resample_size = input$resample_size),
               message = "Calculating...")
})

# Warning about low dose asymptote
observeEvent(c(input$run_analysis, input$pod_ql), {
  dose_check <- sort(unique(dr_dat()$dose))[2]
  pod_check <- quantile(res()$pods$dose, input$pod_ql)
  if (pod_check < dose_check) {
    shinyalert(title = "WARNING",
               text  = p(strong("POD Credible Interval violates low-dose 
                                 asymptote assumption.\n"),
                         br(),
                         "POD estimation assumes that the input dose-response
                          data are asymptotic at low-doses. The lower limit of
                          the POD Credible Interval is within the two smallest
                          doses. Verify that an asymptote has been established
                          at lower doses of the input data."),
               type  = "warning",
               html  = TRUE,
               size  = "m")
  }
})

### Display Results -----
# Display result table upon action button click
pod_res_table <- eventReactive(input$run_analysis, {
  pod_res_temp <- res()$pods[,c("bs_index", "dose", "mc")]
  pod_res_temp[,c("dose", "mc")] <- apply(pod_res_temp[,c("dose", "mc")], 2, my_round)
  datatable(pod_res_temp,
            colnames = c("Bootstrap Index", "POD", "Menger Curvature"),
            rownames = FALSE,
            options  = list(dom = "tlp"))
})
output$pod_table <- output$bs_table <- renderDataTable({
  pod_res_table()
})

# Set plot parameters
pe_plot_opts <- reactive({scale_params(input$viewopt_pod_estimate)})
pod_plot_opts <- reactive({req(res())
                           pod_params(pod_df      = res()$pods,
                                      in_dat      = dr_dat(),
                                      scale_opts  = pe_plot_opts(),
                                      pod_qs      = c(input$pod_ql, input$pod_qu),
                                      show_median = input$med_line,
                                      show_mean   = input$mean_line)
})

# Draw POD distribution
output$pod_dist <- renderPlot({plot_pod_dist(pod_df     = res()$pods,
                                             scale_opts = pe_plot_opts(),
                                             pod_opts   = pod_plot_opts(),
                                             x_ticks    = x_ticks())
})

# Draw bootstrap summary plot 
output$bs_summary_plot <- renderPlot({plot_mc_summary(spline_df  = res()$spline_predictions,
                                                      pod_df     = res()$pods, 
                                                      scale_opts = pe_plot_opts(),
                                                      pod_opts   = pod_plot_opts(),
                                                      x_ticks    = x_ticks())
}, height = 500)

### Save results -----
output$downloadRes <- downloadHandler(
  filename = function() {
    paste0(format(Sys.time(), "%Y%m%d_%H%M_toxpodoutput.zip"))
  },
  content = function(file){
    dir   <- getwd()
    setwd(tempdir()); on.exit(setwd(dir))
    files <- c("input_data.txt",
               "analysis_parameters.txt",
               "pod_results_intervals.txt",
               "pod_results_all.txt",
               "spline_results.txt",
               "pod_dist.pdf",
               "bs_summary.pdf")

    # Input data
    in_dat <- data.frame(dr_dat(), row.names = NULL)
    write.table(in_dat, files[1], row.names = F, sep = "\t", quote = FALSE)

    # Analysis options
    params <- c()
    if (input$data_source == "User Data"){
      if(input$user_input_choice == "Upload"){
        params <- c(params, 
                    "Data Source" = "User Upload",
                    "File Path"   = input$user_fpath$name)
      } else if (input$user_input_choice == "Paste"){
        params <- c(params, "Data Source" = "User Pasted")
      }
    } else if (input$data_source == "Example") {
      params <- c(params,
                  "Data Source"  = "Example",
                  "Example File" = input$example_choice)
    }
    params <- c(params, "Number of Bootstrap Samples" = input$resample_size)
    params <- data.frame(Parameter = names(params),
                         Value     = params,
                         row.names = NULL)
    write.table(params, files[2], row.names = F, sep = "\t", quote = FALSE)

    # Quantiles
    quants <- sort(unique(c(input$pod_ql, input$pod_qu, 0.025, 0.975, 0.5)))
    quant_res <- quantile(res()$pods$dose, quants)
    quant_res <- data.frame(Quantile = names(quant_res), POD = quant_res)
    write.table(quant_res, files[3], row.names = F, sep = "\t", quote = FALSE)

    # POD Results
    pod_res <- res()$pods
    colnames(pod_res) <- c("bs_index", "pod", "log10_pod", "mc")
    write.table(pod_res, files[4], row.names = F, sep = "\t", quote = FALSE)

    # Spline and Curvature Results
    spline_res <- res()$spline_predictions
    write.table(spline_res, files[5], row.names = F, sep = "\t", quote = FALSE)

   # POD Distribution
    pdf(files[6], width = 8.5, height = 6)
    plot_pod_dist(pod_df     = res()$pods,
                  scale_opts = pe_plot_opts(),
                  pod_opts   = pod_plot_opts(),
                  x_ticks    = x_ticks())
    dev.off()

    # Bootstrap Summary
    pdf(files[7], width = 11, height = 7)
    plot_mc_summary(spline_df  = res()$spline_predictions,
                    pod_df     = res()$pods,
                    scale_opts = pe_plot_opts(),
                    pod_opts   = pod_plot_opts(),
                    x_ticks    = x_ticks())
    dev.off()

    zip(file, files)
  },
  contentType = "application/zip"
)