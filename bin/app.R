# Load shiny package
library(shiny)
library(DT)
source("functions/functions.R")
source("functions/make_plots.R")

# Define UI 
source("ui/pod_estimation_ui.R")
source("ui/data_explorer_ui.R")
source("ui/about_ui.R")

ui <- fluidPage(
  theme = "bootstrap.css",
  includeCSS("../www/flatly_theme.css"),
  navbarPage(
  title = "GRAVEE",
  id = "nav_bar",
  tabPanel("POD Estimation", icon = icon("play"),
           column(4,
                  pod_data_panel,
                  pod_graph_panel,
                  pod_analyze_panel,
                  pod_dl_panel),
           column(8,
                  tabsetPanel(id = "POD_Panel",
                              input_data_panel,
                              result_panel))),
  tabPanel("Sample Explorer", icon = icon("chart-line"),
           column(4,
                  bs_view_opts,
                  bs_sample_panel),
           column(8,
                  tabsetPanel(id = "explorer_panel",
                              bs_summary_main,
                              bs_sample_main))),
  tabPanel("About", icon = icon("info-circle"),
           fluidRow(
             about_header,
             instructions_section,
             in_example_section,
             analysis_section,
             sample_ex_section,
             license_section,
             contact_section
           ))
))

server <- function(input, output, session){
  
  source("server/pod_estimation_server.R", local = T)
  source("server/data_explorer_server.R", local = T)
  source("server/about_server.R", local = T)
  
}

shinyApp(ui, server)