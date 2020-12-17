################################
# Server for displaying bootstrap samples
################################

# Draw bootstrap summary plot upon action button click
bs_summary_plot <- eventReactive(input$viewopt_bs,
                                 plot_mc_summary(res()$spline_predictions, res()$pods, dr_dat(), input$viewopt_bs))

observeEvent(input$viewopt_bs, {
  output$bs_summary_plot <- withProgress(renderPlot(bs_summary_plot()), message = "Drawing Summary Plot...")
})

# User selects individual bootstrap samples to plot
observe({
  choices <- res()$pods$bs_index
  updateSelectInput(session, "bs_id", choices = choices)
})

# Draw bootstrap plots
bs_plots <- eventReactive(c(input$plot_bs, input$viewopt_bs),
                          plot_mc(res()$spline_predictions, res()$menger_curvature, res()$pods, bs_id = as.numeric(input$bs_id), dr_dat(), input$viewopt_bs))

observeEvent(input$plot_bs, {
  output$bs_plots <- withProgress(renderPlot(bs_plots()), message = "Drawing Sample Plots...")
})

# Switch tabs
observeEvent(input$plot_bs, {
  updateTabsetPanel(session, "explorer_panel", selected = "Bootstrap Samples")
})
