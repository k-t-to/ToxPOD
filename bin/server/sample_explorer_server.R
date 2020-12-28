################################
# Server for displaying bootstrap samples
################################

# Sidebar options ----- 
# Update bootstrap choices from analysis
observe({
  choices <- res()$pods$bs_index
  updateSelectInput(session, "bs_id", choices = choices)
})

# Update max number of samples to choose
observe({
  n_bs <- input$resample_size
  updateNumericInput(session, inputId = "random_sample_select", max = n_bs)
})

# Clear selected samples
observeEvent(input$reset_bs_samples, {
  updateSelectInput(session, "bs_id", selected = NA)
})

# Sample Selection 
bs_id <- reactiveVal()

# Update bs_id when Select Input is given values
observeEvent(input$bs_id, {
  bs_id(input$bs_id)
})

# When one of the choices is selected
observe({
  if (input$data_explorer_sample_choice == "All") {
    bs_id(res()$pods$bs_index)
  }
})

observeEvent(input$random_sample_select, {
  if (!is.na(input$random_sample_select)){
    n <- sort(sample(input$resample_size, size = input$random_sample_select))
    bs_id(n)
    updateSelectInput(session, "bs_id", selected = n)}
},)

# Individual Plots ----- 
# Get height of plot to draw
bs_plot_dims <- reactive({
  c(750,
    ceiling(length(bs_id())/2 * (750/3)))
  
})

# Individual sample plots 
bs_plots <- eventReactive(
  c(input$plot_bs_samples, input$viewopt_data_explorer),
  {
    if (input$data_explorer_sample_choice != "All") {
      plot_mc(res()$spline_predictions, res()$menger_curvature, res()$pods, bs_id = as.numeric(bs_id()), dr_dat(), input$viewopt_data_explorer)
    }
  }
)

observeEvent(input$plot_bs_samples, {
  output$bs_plots <- withProgress(renderPlot(bs_plots(), width = bs_plot_dims()[1], height = bs_plot_dims()[2]), message = "Drawing Sample Plots...")
})

# Switch tabs
observeEvent(input$plot_bs_samples, {
  updateTabsetPanel(session, "explorer_panel", selected = "bs_sample_tab")
})

# Summary Plots ----- 
bs_resample_plot <- eventReactive(c(input$plot_bs_summary, input$viewopt_data_explorer),
                                  plot_bs(res()$bootstrap_values, bs_id(), input$viewopt_data_explorer))
bs_splinefit_plot <- eventReactive(c(input$plot_bs_summary, input$viewopt_data_explorer),
                                   plot_splines(res()$spline_predictions, bs_id(), input$viewopt_data_explorer))

observeEvent(input$plot_bs_summary, {
  output$resample_plot <- withProgress(renderPlot(bs_resample_plot()), message = "Drawing Bootstrap Samples...")
  output$bs_splinefit_plot <- withProgress(renderPlot(bs_splinefit_plot()), message = "Drawing Spline Predictions...")
})

observeEvent(input$plot_bs_summary, {
  updateTabsetPanel(session, "explorer_panel", selected = "bs_summary_tab")
})
