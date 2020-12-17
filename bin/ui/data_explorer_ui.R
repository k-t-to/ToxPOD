################################
# UI for Bootstrap Summary
################################

### Sidebar ----- 
# Summary selections
bs_run_panel <- conditionalPanel(
  condition = "input.Run",
  wellPanel(
    actionButton(inputId = "bs_summary",
                 label = "Draw Summary Plot"),
    hr(),
    selectInput(inputId = "bs_id",
                label = "Bootstrap Samples",
                choices = c(),
                multiple = TRUE),
    actionButton(inputId = "plot_bs",
                 label = "Draw Plots")
  )
)

### Main panel -----
bs_panel <- tabPanel(
  "Bootstrap Summary",
  fluidRow(
    column(12,
           plotOutput("bs_summary_plot")),
    hr(),
    fluidRow(column(12,
                    plotOutput("bs_plots")))
  )
)