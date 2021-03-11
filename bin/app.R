# Load shiny package
if (!require("shiny")) install.packages("shiny"); library("shiny")
if (!require("shinyalert")) install.packages("shinyalert"); library("shinyalert")
if (!require("DT")) install.packages("DT"); library("DT")
source("functions/functions.R")
source("functions/make_plots.R")

# Define UI 
source("ui/data_input_ui.R")
source("ui/pod_estimation_ui.R")
source("ui/sample_explorer_ui.R")

ui <- fluidPage(
  theme = "bootstrap.css",
  includeCSS("../www/flatly_theme.css"),
  useShinyalert(),
  navbarPage(
  title = "ToxPOD",
  id = "nav_bar",
  input_data_tab,
  pod_estimate_tab,
  data_explorer_tab,
  tabPanel("About", icon = icon("info-circle"),
           fluidRow(
             column(8,
                    includeMarkdown("../README.md"),
                    offset = 2)))
))

server <- function(input, output, session){
  
  source("server/data_input_server.R", local = T)
  source("server/pod_estimation_server.R", local = T)
  source("server/sample_explorer_server.R", local = T)
  
}

shinyApp(ui, server)