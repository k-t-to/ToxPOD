################################
# UI for Bootstrap Summary
################################

### Sidebar ----- 
# View options
data_explorer_viewopts_sidebar <- wellPanel(
  p(strong("View Options"), align = "center", style = "color:#919aa1; text-transform: uppercase; padding:0px; margin:0px"),
  hr(style = "border-color:#919aa1; padding-top:2px; margin-top:5px; margin-bottom:5px"),
  radioButtons(inputId = "viewopt_data_explorer",
               label = "Dose Scale",
               choices = c("Log\u2081\u2080(Dose + 1)" = "Log10(Doses)",
                           "Original Dose" = "Original Doses"),
               selected = "Log10(Doses)"))

# Summary selections
data_explorer_sample_choice_sidebar <- wellPanel(
  radioButtons(inputId = "data_explorer_sample_choice",
               label = "Samples to Plot",
               choices = c("Select Samples",
                           "Select Random",
                           "All")),
  conditionalPanel(
    condition = "input.data_explorer_sample_choice == 'Select Random'",
    numericInput(inputId = "random_sample_select",
                 label = "Number of Samples to Plot",
                 min = 0,
                 value = 0)
  ),
  conditionalPanel(
    condition = "input.data_explorer_sample_choice !== 'All'",
    selectInput(inputId = "bs_id",
                label = "Samples to Plot",
                choices = c(),
                selected = NULL,
                multiple = TRUE),
    actionButton(inputId = "reset_bs_samples",
                 label = "Reset Selected Samples",
                 class = "btn btn-secondary btn-sm")),
  hr(style = "border-color:#919aa1;"),
  conditionalPanel(
    condition = "input.data_explorer_sample_choice !== 'All'",
    actionButton(inputId = "plot_bs_samples",
                 label = "Draw Individual Plots",
                 class = "btn btn-primary")),
  div(style="margin-bottom:10px"),
  actionButton(inputId = "plot_bs_summary",
               label = "Draw Summary Plots",
               class = "btn btn-primary"))

### Main panel -----
data_explorer_sample_main <- tabPanel(
  title = "Individual Plots",
  value = "bs_sample_tab",
  fluidRow(column(12,
           plotOutput("bs_plots")))
)

data_explorer_summary_main <- tabPanel(
  title = "Summary Plots",
  value = "bs_summary_tab",
  fluidRow(column(12,
                  plotOutput("resample_plot"),
                  plotOutput("bs_splinefit_plot")))
)

# Tab Panel ----- 
data_explorer_tab <- tabPanel(
  title = "Sample Explorer",
  icon = icon("table"),
  conditionalPanel(
    condition = "input.run_analysis",
    fluidRow(
      column(4,
             data_explorer_sample_choice_sidebar,
             data_explorer_viewopts_sidebar),
      column(8,
             tabsetPanel(id = "explorer_panel",
                         data_explorer_sample_main,
                         data_explorer_summary_main)))))
