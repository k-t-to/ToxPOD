################################
# Server for displaying bootstrap samples
################################

### Sample Selection -----
# Update sample numbers
observeEvent(input$run_analysis, {
  n_bs <- input$resample_size
  updateSelectInput(session, "bs_id", choices = 1:n_bs)
  updateNumericInput(session, inputId = "random_sample_select", max = n_bs)
})

# Clear selected samples
observeEvent(input$reset_bs_samples, {
  updateSelectInput(session, "bs_id", selected = NA)
})

# Update selected samples
bs_id <- reactiveVal()
observe({
  if (input$data_explorer_sample_choice == "user") bs_id(input$bs_id)
  if (input$data_explorer_sample_choice == "random") {
    vals <- sort(sample(input$resample_size, size = input$random_sample_select))
    bs_id(vals)
    updateSelectInput(session, "bs_id", selected = vals)
  }
  if (input$data_explorer_sample_choice == "all") bs_id(1:input$resample_size)
})

### Plots -----
# Set plot parameters
se_plot_opts <- reactive({scale_params(input$viewopt_data_explorer)})
h <- eventReactive(input$plot_bs_samples, {
  n <- length(bs_id())
  if (n > 2){
    (ceiling(n/2) * 250) + 30
  } else {280}
})

# Display individual sample plots
bs_plots <- eventReactive(c(input$plot_bs_samples, se_plot_opts()), {
  plot_mc(spline_df  = res()$spline_predictions,
          pod_df     = res()$pods,
          bs_id      = as.numeric(bs_id()),
          scale_opts = se_plot_opts(),
          x_ticks    = x_ticks())
})
output$bs_plots <- renderPlot(bs_plots(), height = function(){h()})

# Display summary plots
bs_resample_plot <- eventReactive(c(input$plot_bs_summary, se_plot_opts()), {
  plot_bs_summary(df         = res()$bootstrap_values,
                  bs_ids     = bs_id(),
                  type       = "bs",
                  scale_opts = se_plot_opts(),
                  x_ticks    = x_ticks())
})
bs_splinefit_plot <- eventReactive(c(input$plot_bs_summary, se_plot_opts()), {
  plot_bs_summary(df         = res()$spline_predictions,
                  bs_ids     = bs_id(),
                  type       = "spline",
                  scale_opts = se_plot_opts(),
                  x_ticks    = x_ticks())
})
observeEvent(input$plot_bs_summary, {
  output$resample_plot <- renderPlot(bs_resample_plot())
  output$bs_splinefit_plot <- renderPlot(bs_splinefit_plot())
})

# Switch tabs
observeEvent(input$plot_bs_samples, {
  updateTabsetPanel(session, "explorer_panel", selected = "bs_sample_tab")
})
observeEvent(input$plot_bs_summary, {
  updateTabsetPanel(session, "explorer_panel", selected = "bs_summary_tab")
})