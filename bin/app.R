# Load shiny package
library(shiny)
library(DT)
source("functions/functions.R")
source("functions/make_plots.R")

# Define UI 
source("pod_ui.R")

ui <- fluidPage(
  # App title ----- 
  titlePanel("GRAVEE", "GRAVEE"),
  helpText(div(h4("Good Risk Assessment Values for Environmental Exposures"),
               h5("Estimating Point of Departure (POD) from dose-response data using spline meta-regression"))),
  # Main Analysis Page
  sidebar_panel,
  main_panel
  )



server <- function(input, output, session){
  
  source("pod_server.R", local = T)
  
}

shinyApp(ui, server)