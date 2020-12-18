################################
# UI for Bootstrap Summary
################################

### Sidebar ----- 
# View options
bs_view_opts <- wellPanel(
  conditionalPanel(condition = "input.Run",
                   radioButtons(inputId = "viewopt_bs",
                                label = "View Options",
                                choices = c("Log\u2081\u2080(Doses)" = "Log10(Doses)",
                                            "Original Doses" = "Original Doses"),
                                selected = "Log10(Doses)")
  ))

# Summary selections
bs_sample_panel <- conditionalPanel(
  condition = "input.Run",
  wellPanel(
    selectInput(inputId = "bs_id",
                label = "Bootstrap Samples",
                choices = c(),
                multiple = TRUE),
    actionButton(inputId = "plot_bs",
                 label = "Draw Bootstrap Samples")
  )
)


### Main panel -----
bs_summary_main <- tabPanel(
  "Bootstrap Summary",
  fluidRow(
    column(12,
           plotOutput("bs_summary_plot"))
  )
)

bs_sample_main <- tabPanel(
  "Bootstrap Samples",
  fluidRow(column(12,
           plotOutput("bs_plots")))
)
