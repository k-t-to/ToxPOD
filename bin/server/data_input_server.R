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

input_data_plot <- eventReactive(c(dr_dat(), input$viewopt_input),
                                 plot_input_data(dr_dat(), input$viewopt_input))

observeEvent(input_data_plot(),
             output$input_data_plot <- renderPlot(input_data_plot()))

# Display input data
input_data_table <- eventReactive(dr_dat(), {
  formatRound(datatable(do.call("rbind", dr_dat()),
                        colnames = c("Dose", "Log\u2081\u2080(Doses)","Response"),
                        rownames = FALSE,
                        options = list(dom = "tlp")),
              columns = "log10_dose",
              digits = 4)
})

observeEvent(input_data_table(),
             output$input_data_table <- renderDataTable({input_data_table()}))
