################################
# Server for Input Data
################################

### Prepare Input Data -----
dat_path <- reactiveVal()
in_type <- reactiveVal()

# File path given
observeEvent(input$user_fpath, {
  dat_path(input$user_fpath$datapath)
  in_type("file")
})

# Example data selected
observeEvent(input$example_choice, {
  tmp <- switch(
    input$example_choice,
    `Example 1` = "../data/input_example_1.txt",
    `Example 2` = "../data/input_example_2.txt",
    `Example 3` = "../data/input_example_3.txt"
  )
  dat_path(tmp)
  in_type("file")
})

# Data pasted into text box
observeEvent(input$load_pasted, {
  dat_path(input$user_pasted_data)
  in_type("str")
})

# Parse input data
dr_dat <- eventReactive(req(dat_path()),
                        parse_data(dat_path(), in_type()))

# Display input data requirements
observeEvent(input$data_info, {
  shinyalert(title = "Data Input Requirements",
             text = '
             <div style="text-align:left;">
             <ul>
             <li>First row contains column names</li>
             <li>Two numeric columns</li>
             <li>Columns ordered as (Dose, Response)</li>
             <li>At least 4 doses</li>
             <li>At least 3 replicates per dose</li>
             </ul>
             </div>',
             html = TRUE)
})

### Display Input Data -----
# Set plot parameters
x_ticks <- eventReactive(dr_dat(),
                         data.frame(dose       = unique(dr_dat()$dose),
                                    log10_dose = unique(dr_dat()$log10_dose),
                                    dose_lab   = my_round(unique(dr_dat()$dose))))
in_plot_opts <- reactive({scale_params(input$viewopt_input)})

# Input data plot
output$input_data_plot <- renderPlot({
  plot_input_data(dr_dat(), in_plot_opts(), x_ticks())
})

# Input data table
input_data_table <- eventReactive(dr_dat(), {
  dr_dat_display <- apply(dr_dat(), 2, my_round)
  datatable(dr_dat_display,
            colnames = c("Dose", "Log\u2081\u2080(Dose)", "Response"),
            rownames = FALSE,
            options  = list(dom = "tlp"))
})
output$input_data_table <- renderDataTable({input_data_table()})

# Create random seed for analysis
observeEvent(dr_dat(), {
  updateNumericInput(session, inputId = "analysis_seed", value = sample.int(1e6, 1))
})