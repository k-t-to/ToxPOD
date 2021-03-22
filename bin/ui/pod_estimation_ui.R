################################
# UI for POD Estimation 
################################

### Sidebar -----
# Analysis options
analysis_opt_sidebar <- wellPanel(
  # Select number of bootstrap samples
  numericInput(inputId = "resample_size",
               label   = "Number of Bootstraps",
               min     = 10,
               value   = 1000),
  checkboxInput(inputId = "user_seed",
                label   = "Manual Seed",
                value   = F),
  conditionalPanel(condition ="input.user_seed",
                   numericInput(inputId = "analysis_seed",
                                label = "Seed",
                                min = 1,
                                max = 1e6,
                                value = sample.int(1e6,1),
                                step = 1)),
  actionButton(inputId = "run_analysis",
               label   = "Run Analysis",
               class   = "btn btn-primary")
)

# Graph Options
pod_estimate_viewopts_sidebar <- wellPanel(
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
  radioButtons(inputId  = "viewopt_pod_estimate",
               label    = "Dose Scale",
               choices  = c("Log\u2081\u2080(Dose)" = "Log10(Doses)",
                            "Original Dose"         = "Original Doses"),
               selected = "Log10(Doses)"),
  p("Center Lines",
    align = "left",
    style = "font-weight:700; margin-bottom:5px;"),
  checkboxInput(inputId = "med_line",
                label   = "Median",
                value   = T),
  div(style = "margin-bottom:-10px"),
  checkboxInput(inputId = "mean_line",
                label   = "Mean",
                value   = F),
  numericInput(inputId = "pod_ql",
               label   = "Credible Interval Lower Bound",
               min     = 0,
               max     = 0.5,
               value   = 0.025,
               step    = 0.025),
  numericInput(inputId = "pod_qu",
               label   = "Credible Interval Upper Bound",
               min     = 0.5,
               max     = 1,
               value   = 0.975,
               step    = 0.025)
)

# Download results
dl_sidebar <- wellPanel(
  tags$style(type = "text/css", 
             "#downloadRes {background-color:#2c3e50;
                            color:#ffffff;
                            border-color:#2c3e50;}
              #downloadRes:hover {background-color:#000000;
                                  border-color:#000000;}"),
  conditionalPanel(condition ="input.run_analysis",
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
           dataTableOutput("pod_table"),
           offset = 2)
  )
)

bootstrap_summary_main <- tabPanel(
  title = "Bootstrap Summary",
  value = "bs_summary_tab",
  fluidRow(
    column(12,
           plotOutput("bs_summary_plot"),
           style = "padding-bottom:30px;
                    height:520px"),
    column(8,
           dataTableOutput("bs_table"),
           offset = 2)
  )
)

### Tab Panel -----
pod_estimate_tab <- tabPanel(
  title = "Analysis",
  icon  = icon("play"),
  fluidRow(
    column(4,
           conditionalPanel(
             condition = "output.input_data_table",
             analysis_opt_sidebar,
             pod_estimate_viewopts_sidebar,
             dl_sidebar)),
    column(8,
           tabsetPanel(pod_result_main,
                       bootstrap_summary_main))
  )
)