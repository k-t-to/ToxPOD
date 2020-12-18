################################
# UI for POD Estimation 
################################

### Sidebar ----- 
# Input data 
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

# Graph options
pod_graph_panel <- wellPanel(
  radioButtons(inputId = "viewopt",
               label = "View Options",
               choices = c("Log\u2081\u2080(Doses)" = "Log10(Doses)",
                           "Original Doses" = "Original Doses")),
               selected = "Log10(Doses)"
)

# POD Estimation Analysis
pod_analyze_panel <- wellPanel(
  conditionalPanel(condition="output.input_data_table",
                   numericInput(inputId = "resample_size",
                                label = "Number of Bootstraps",
                                min = 10,
                                value = 500),
                   actionButton(inputId = "Run",
                                label = "Run Analysis"))
)

# Download results panel
pod_dl_panel <- wellPanel(
  conditionalPanel(condition="input.Run",
                   downloadButton("downloadRes", "Download Results"))
)

### Main Panel -----
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
