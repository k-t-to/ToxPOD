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
})

# Individual Plots ----- 
calc_h <- function(n) {
  if(n > 2){
    (ceiling(n/2) * 400) + 30
  } else {500}
}

viewopt_de <- reactiveVal("Log10(Doses)")

observeEvent(input$viewopt_data_explorer, {
  viewopt_de(input$viewopt_data_explorer)
})

h <- eventReactive(input$plot_bs_samples, {
  calc_h(length(bs_id()))
})

bs_plots <- eventReactive(c(input$plot_bs_samples, viewopt_de()), {
      plot_mc(res()$spline_predictions, res()$menger_curvature, res()$pods, bs_id = as.numeric(bs_id()), dr_dat(), viewopt_de())
  }
)

output$bs_plots <- renderPlot(bs_plots(), height = function(){h()})

# Switch tabs
observeEvent(input$plot_bs_samples, {
  updateTabsetPanel(session, "explorer_panel", selected = "bs_sample_tab")
})

# Summary Plots ----- 

bs_resample_plot <- eventReactive(c(input$plot_bs_summary, input$viewopt_data_explorer),
                                  plot_bs(res()$bootstrap_values, bs_id(), dr_dat(), input$viewopt_data_explorer))
bs_splinefit_plot <- eventReactive(c(input$plot_bs_summary, input$viewopt_data_explorer),
                                   plot_splines(res()$spline_predictions, bs_id(), dr_dat(), input$viewopt_data_explorer))

observeEvent(input$plot_bs_summary, {
  output$resample_plot <- withProgress(renderPlot(bs_resample_plot()), message = "Drawing Bootstrap Samples...")
  output$bs_splinefit_plot <- withProgress(renderPlot(bs_splinefit_plot()), message = "Drawing Spline Predictions...")
})

observeEvent(input$plot_bs_summary, {
  updateTabsetPanel(session, "explorer_panel", selected = "bs_summary_tab")
})
