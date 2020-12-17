################################
# Server for displaying bootstrap samples
################################

# Draw bootstrap summary plot upon action button click
bs_summary_plot <- eventReactive(input$bs_summary,
                                 plot_mc_summary(res()$spline_predictions, res()$pods))

observeEvent(input$bs_summary, {
  output$bs_summary_plot <- withProgress(renderPlot(bs_summary_plot()), message = "Drawing Summary Plot...")
})

# User selects individual bootstrap samples to plot
observe({
  choices <- res()$pods$bs_index
  updateSelectInput(session, "bs_id", choices = choices)
})

# Draw bootstrap plots
bs_plots <- eventReactive(input$plot_bs,
                          plot_mc(res()$spline_predictions, res()$menger_curvature, res()$pods, bs_id = as.numeric(input$bs_id)))

observeEvent(input$plot_bs, {
  output$bs_plots <- withProgress(renderPlot(bs_plots()), message = "Drawing Sample Plots...")
})

# Switch to Bootstrap summary tab after action button click
observeEvent(input$bs_summary, {
  updateTabsetPanel(session, "POD_Panel", selected = "Bootstrap Summary")
})

observeEvent(input$plot_bs, {
  updateTabsetPanel(session, "POD_Panel", selected = "Bootstrap Summary")
})