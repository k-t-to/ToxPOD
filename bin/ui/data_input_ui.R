################################
# UI for Input Data
################################

### Sidebar -----
# Input data
input_data_sidebar <- wellPanel(
  # Select Data Source
  radioButtons(inputId  = "data_source",
               label    = "Select Data Source",
               choices  = c("User Data", "Example"),
               selected = "User Data"),

  # If User Data, upload or hard enter?
  conditionalPanel(condition = "input.data_source == 'User Data'",
                   radioButtons(inputId  = "user_input_choice",
                                label    = "Data Input Method",
                                choices  = c("Upload", "Paste"),
                                selected = "Upload")),

  # If Upload, pull up upload panel
  tags$style(type = "text/css",
             ".btn-file {background-color:#2c3e50;
                         color:#ffffff;
                         border-color:#2c3e50;}
              .btn-file:hover {background-color:#000000;
                               border-color:#000000}"),
  conditionalPanel(condition = "input.data_source == 'User Data' &
                                input.user_input_choice == 'Upload'",
                   fileInput(inputId  = "user_fpath",
                             label    = "Select File",
                             multiple = FALSE)),

  # If Paste, pull up input text panel
  conditionalPanel(condition = "input.data_source == 'User Data' &
                                input.user_input_choice == 'Paste'",
                   textAreaInput(inputId = "user_pasted_data",
                                 label   = "Paste Data"),
                   actionButton(inputId = "load_pasted",
                                label   = "Load Data",
                                class   = "btn btn-primary")),

  # If example, pull up example options
  conditionalPanel(condition = "input.data_source == 'Example'",
                   radioButtons(inputId  = "example_choice",
                                label    = "Select Example Data",
                                choices  = c("Pretty"        = "Example 1",
                                             "High Variance" = "Example 2",
                                             "No Asymptote"  = "Example 3"),
                                selected = "Poor Data")),
  div(style = "margin-bottom:-5px"),

  # Show input data requirements
  actionButton(inputId = "data_info",
               label   = "Data Requirements",
               class   = "btn btn-link",
               style   = "margin-left:-10px"),
  div(style = "margin-bottom:-10px")
)

# Graph options
input_view_opts_sidebar <- wellPanel(
  p(strong("View Options"),
    align = "center",
    style = "color:#919aa1;
             text-transform: uppercase;
             padding:0px;
             margin:0px"),
  hr(style = "border-color:#919aa1;
              padding-top:2px;
              margin-top:5px;
              margin-bottom:5px"),
  radioButtons(inputId  = "viewopt_input",
               label    = "Dose Scale",
               choices  = c("Log\u2081\u2080(Dose)" = "Log10(Doses)",
                            "Original Dose"         = "Original Doses"),
               selected = "Log10(Doses)"),
)

### Main Panel -----
input_data_main <- fluidRow(
  column(12,
         plotOutput("input_data_plot"),
         style = "padding-top:20px; padding-bottom:20px"),
  column(6,
         dataTableOutput("input_data_table"),
         offset = 3)
)

### Tab Panel ----- 
input_data_tab <- tabPanel(
  title = "Input Data",
  icon  = icon("database"),
  fluidRow(
    column(4,
           input_data_sidebar,
           input_view_opts_sidebar),
    column(8,
           input_data_main)
  )
)