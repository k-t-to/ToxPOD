# UI for POD Estimation Tab

# POD Estimation Tab -----
# Sidebar 
pod_data_panel <- wellPanel(
  radioButtons(inputId = "data_choice",
               label = "Data Source",
               choices = c("User Data", "Example 1", "Example 2", "Example 3"),
               selected = "User Data"),
  conditionalPanel(condition = "input.data_choice == 'User Data'",
                   fileInput(inputId = "input_file",
                             label = "Upload File",
                             multiple = FALSE))
)

pod_analyze_panel <- wellPanel(
  conditionalPanel(condition="output.input_data_table",
                   numericInput(inputId = "resample_size",
                                label = "Number of Bootstraps",
                                min = 10,
                                value = 500),
                   actionButton(inputId = "Run",
                                label = "Run Analysis"))
)

pod_dl_panel <- wellPanel(
  conditionalPanel(condition="input.Run",
                   downloadButton("downloadRes", "Download Results"))
)

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

sidebar_panel <- column(4,
                        pod_data_panel,
                        pod_analyze_panel,
                        bs_run_panel,
                        pod_dl_panel)

# Main Panel
input_data_panel <- tabPanel(
  "Input Data",
  fluidRow(
    column(12, 
           plotOutput("input_data_plot"),
           style = "padding-top:20px; padding-bottom:20px"),
    column(6,
           dataTableOutput("input_data_table"),
           offset = 3)))


result_panel <- tabPanel(
  "Results",
  fluidRow(
    column(12,
           plotOutput("pod_dist"),
           style = "padding-bottom:30px"),
    column(8,
           dataTableOutput("table"),
           offset = 2)
  )
)

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


main_panel <- column(8,
         tabsetPanel(id = "POD_Panel",
                     input_data_panel,
                     result_panel,
                     bs_panel))
  

# pod_tab_layout <- sidebarLayout(pod_sidepanel, pod_mainpanel)
# pod_tab_layout <- pod_sidepanel