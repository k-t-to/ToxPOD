################################
# Server for POD Estimation 
################################

### Input Data -----
# Load and parse file based on user input
user_dat <- eventReactive(input$input_file, parse_data(input$input_file$datapath))
ex_dat <- eventReactive(input$data_choice %in% c("Example 1", "Example 2", "Example 3"), {
  if (input$data_choice == "Example 1") {return(parse_data("../data/input_example_1.txt"))}
  if (input$data_choice == "Example 2") {return(parse_data("../data/input_example_2.txt"))}
  if (input$data_choice == "Example 3") {return(parse_data("../data/input_example_3.txt"))}
})

dr_dat <- reactive({
  if (!is.null(ex_dat())) {
    ex_dat()
  } else {
    user_dat()
  }
})

# Plot input data

input_data_plot <- eventReactive(c(dr_dat(), input$viewopt),
                                 plot_input_data(dr_dat(), input$viewopt))

observeEvent(input_data_plot(),
             output$input_data_plot <- renderPlot(input_data_plot()))

# Display input data
input_data_table <- eventReactive(dr_dat(), {
  formatRound(datatable(do.call("rbind", dr_dat()),
                        colnames = c("dose", "log10_dose","response"),
                        rownames = FALSE,
                        options = list(dom = "tlp")),
              columns = "log10_dose",
              digits = 4)
})

observeEvent(input_data_table(),
             output$input_data_table <- renderDataTable({input_data_table()}))

# Switch back to input data tab when data choice is updated
observeEvent(input$data_choice, {
  updateTabsetPanel(session, "POD_Panel", selected = "Input Data")
})

### Analysis ---- 
# User clicks Run Analysis
res <- eventReactive(input$Run, {
  withProgress(calculate_pod_quantiles(dr_dat(), resample_size = input$resample_size), message = "Calculating...")
})

# Switch tab to Results upon action button click
observeEvent(input$Run, {
  updateTabsetPanel(session, "POD_Panel", selected = "Results")
})

# Display result table upon action button click
pod_res_table <- eventReactive(input$Run, {
  datatable(res()$pods,
            colnames = c("Bootstrap Index", "POD (Original Scale)", "POD (Log10 Scale)", "Menger Curvature"),
            rownames = FALSE,
            options = list(dom = "tlp"))
})

# Display results table
output$table <- renderDataTable({
  formatRound(pod_res_table(), columns = c(2,3,4), digits = 2)
})

# Draw plot of pod distribution
pod_dist_dose <- eventReactive(input$Run,
                               plot_pod_dist(res()$pods, res()$pod_quantile, dr_dat(), "Original Doses"))

pod_dist_log <- eventReactive(input$Run,
                               plot_pod_dist(res()$pods, res()$pod_quantile, dr_dat(), "Log10(Doses)"))

# output$pod_dist <- renderPlot(pod_dist_plot())

observeEvent(input$viewopt, {
  if (input$viewopt == "Original Doses") {
    output$pod_dist <- renderPlot(pod_dist_dose())
  } else if (input$viewopt == "Log10(Doses)") {
    output$pod_dist <- renderPlot(pod_dist_log())
  }
})


# Save results 
output$downloadRes <- downloadHandler(
  filename = function() {
    paste0(format(Sys.time(), "%Y%m%d_%H%M_graveeoutput.zip"))
  },
  content = function(file){
    dir   <- getwd()
    setwd(tempdir()); on.exit(setwd(dir))
    files <- c("pod_results.txt", "mc_results.txt", "pod_dist.png", "pod_dist_log.png")
    # files <- c("pod_results.txt", "mc_results.txt")
    # pod results
    pod_res <- res()$pods
    colnames(pod_res) <- c("bs_index", "pod", "log10_pod", "mc")
    write.table(pod_res, files[1], row.names = F, sep = "\t", quote = FALSE)
    # spline + mc results
    mc_res <- res()$menger_curvature
    spline_res <- res()$spline_predictions
    spline_res$mc <- mc_res$mc[match(interaction(spline_res$bs_index, spline_res$dose), interaction(mc_res$bs_index, mc_res$dose))]
    write.table(spline_res, files[2], row.names = F, sep = "\t", quote = FALSE)
    # pod distribution
    ggplot2::ggsave("pod_dist.png", pod_dist_dose(), width = 8, height = 4, unit = "in")
    ggplot2::ggsave("pod_dist_log.png", pod_dist_log(), width = 8, height = 4, unit = "in")
    zip(file, files)
  },
  contentType = "application/zip"
)
