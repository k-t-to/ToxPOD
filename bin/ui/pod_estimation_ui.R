################################
# UI for POD Estimation 
################################

### Sidebar ----- 
# Input data 
# POD Estimation Analysis
pod_estimate_viewopts_sidebar <- wellPanel(
                   radioButtons(inputId = "viewopt_pod_estimate",
                                label = "View Options",
                                choices = c("Log\u2081\u2080(Doses)" = "Log10(Doses)",
                                            "Original Doses" = "Original Doses"),
                                selected = "Log10(Doses)"))

analysis_opt_sidebar <- wellPanel(
                   numericInput(inputId = "resample_size",
                                label = "Number of Bootstraps",
                                min = 10,
                                value = 500),
                   actionButton(inputId = "run_analysis",
                                label = "Run Analysis",
                                class = "btn btn-primary"))


# Download results panel
dl_sidebar <- wellPanel(
  tags$style(type = "text/css", 
             "#downloadRes {background-color:#2c3e50; color:#ffffff; border-color:#2c3e50;} #downloadRes:hover {background-color:#000000; border-color:#000000;}"),
  conditionalPanel(condition="input.run_analysis",
                   downloadButton("downloadRes", "Download Results"))
)

### Main Panel -----
pod_result_main <- tabPanel(
  title = "POD Estimates",
  fluidRow(
    column(12,
           plotOutput("pod_dist"),
           style = "padding-bottom:30px"),
    column(8,
           dataTableOutput("table"),
           offset = 2)
  )
)

bootstrap_summary_main <- tabPanel(
  title = "Bootstrap Summary",
  value = "bs_summary_tab",
  fluidRow(
    column(12,
           plotOutput("bs_summary_plot"))
  )
)
  

pod_estimate_tab <- tabPanel(
  title = "Analysis",
  icon = icon("play"),
  fluidRow(
    column(4,
           conditionalPanel(
             condition = "output.input_data_table",
             analysis_opt_sidebar,
             pod_estimate_viewopts_sidebar,
             dl_sidebar)),
    column(8,
           tabsetPanel(pod_result_main,
                       bootstrap_summary_main))))
