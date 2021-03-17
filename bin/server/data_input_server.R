################################
# Server for Input Data
################################

### Input Data -----
# Load and parse file based on user input

dat_path <- reactiveVal()
in_type <- reactiveVal()

observeEvent(input$user_fpath,{
  dat_path(input$user_fpath$datapath)
  in_type("file")
  })

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

observeEvent(input$load_pasted, {
  dat_path(input$user_pasted_data)
  in_type("str")
})

dr_dat <- eventReactive(req(dat_path()), parse_data(dat_path(), in_type()))

# Plot input data
output$input_data_plot <- renderPlot({plot_input_data(dr_dat(), input$viewopt_input)})

# Display input data
input_data_table <- eventReactive(dr_dat(), {
  dr_dat_display <- do.call("rbind", dr_dat())
  dr_dat_display <- apply(dr_dat_display, 2, function(x){ifelse(abs(x) < 0.01, signif(x, digits = 2), round(x, digits = 2))})
  datatable(dr_dat_display,
                        colnames = c("Dose", "Log\u2081\u2080(Dose)","Response"),
                        rownames = FALSE,
                        options = list(dom = "tlp"))
})

observeEvent(input_data_table(),
             output$input_data_table <- renderDataTable({input_data_table()}))

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
