### Input Data -----

# Data choice
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

# Display input data
input_data_plot <- eventReactive(dr_dat(),
                                 plot_input_data(dr_dat()))

observeEvent(input_data_plot(),
             output$input_data_plot <- renderPlot(input_data_plot()))

# Display input table
input_data_table <- eventReactive(dr_dat(), {
  datatable(do.call("rbind", dr_dat()),
            colnames = c("dose", "response"),
            rownames = FALSE,
            options = list(dom = "tlp"))
})

observeEvent(input_data_table(),
             output$input_data_table <- renderDataTable({input_data_table()}))

observeEvent(input$data_choice, {
  updateTabsetPanel(session, "POD_Panel", selected = "Input Data")
})

### Analysis ---- 
# User clicks action button
# Results calculated
res <- eventReactive(input$Run, {
  withProgress(calculate_pod_quantiles(dr_dat(), resample_size = input$resample_size), message = "Calculating...")
})

# Switch tab
observeEvent(input$Run, {
  updateTabsetPanel(session, "POD_Panel", selected = "Results")
})

# Create result table upon action button click
pod_res_table <- eventReactive(input$Run, {
  datatable(res()$pods,
            colnames = c("Bootstrap Index", "POD", "Menger Curvature"),
            rownames = FALSE,
            options = list(dom = "tlp"))
})

# Render data table to display
output$table <- renderDataTable({
  formatRound(pod_res_table(), columns = c(2,3), digits = 2)
})

# Draw plot of pod distribution
pod_dist_plot <- eventReactive(input$Run, {
  plot_pod_dist(res()$pods, res()$pod_quantile)
})

# Render plot to display
output$pod_dist <- renderPlot(pod_dist_plot())

# To save output... update
output$downloadRes <- downloadHandler(
  filename = function() {
    paste0(format(Sys.time(), "%Y%m%d_%H%M_graveeoutput.zip"))
  },
  content = function(file){
    dir   <- getwd()
    setwd(tempdir()); on.exit(setwd(dir))
    files <- c("pod_results.txt", "mc_results.txt", "pod_dist.png")
    # files <- c("pod_results.txt", "mc_results.txt")
    # pod results
    pod_res <- res()$pods
    colnames(pod_res) <- c("bs_index", "pod", "mc")
    write.table(pod_res, files[1], row.names = F, sep = "\t", quote = FALSE)
    # spline + mc results
    mc_res <- res()$menger_curvature
    spline_res <- res()$spline_predictions
    spline_res$mc <- mc_res$mc[match(interaction(spline_res$bs_index, spline_res$dose), interaction(mc_res$bs_index, mc_res$dose))]
    write.table(spline_res, files[2], row.names = F, sep = "\t", quote = FALSE)
    # pod distribution
    ggplot2::ggsave("pod_dist.png", pod_dist_plot(), width = 8, height = 4, unit = "in")
    zip(file, files)
  },
  contentType = "application/zip"
)

bs_summary_plot <- eventReactive(input$bs_summary,
                                 plot_mc_summary(res()$spline_predictions, res()$pods))

observeEvent(input$bs_summary, {
  output$bs_summary_plot <- withProgress(renderPlot(bs_summary_plot()), message = "Drawing Summary Plot...")
})

observe({
  choices <- res()$pods$bs_index
  updateSelectInput(session, "bs_id", choices = choices)
})

bs_plots <- eventReactive(input$plot_bs,
                          plot_mc(res()$spline_predictions, res()$menger_curvature, res()$pods, bs_id = as.numeric(input$bs_id)))

observeEvent(input$plot_bs, {
  output$bs_plots <- withProgress(renderPlot(bs_plots()), message = "Drawing Sample Plots...")
})

observeEvent(input$bs_summary, {
  updateTabsetPanel(session, "POD_Panel", selected = "Bootstrap Summary")
})

observeEvent(input$plot_bs, {
  updateTabsetPanel(session, "POD_Panel", selected = "Bootstrap Summary")
})