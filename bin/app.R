# Load shiny package
library(shiny)
library(DT)
source("functions/functions.R")
source("functions/make_plots.R")

# Define UI 
source("ui/pod_estimation_ui.R")
source("ui/data_explorer_ui.R")

ui <- fluidPage(
  # App title ----- 
  titlePanel("GRAVEE", "GRAVEE"),
  helpText(div(h4("Good Risk Assessment Values for Environmental Exposures"),
               h5("Estimating Point of Departure (POD) from dose-response data using spline meta-regression"))),
  # Main Analysis Page
  # Sidebar Panel 
  column(4,
         pod_data_panel,
         pod_analyze_panel,
         bs_run_panel,
         pod_dl_panel),
  # Main panel 
  column(8,
         tabsetPanel(id = "POD_Panel",
                     input_data_panel,
                     result_panel,
                     bs_panel))
  )


server <- function(input, output, session){
  
  source("server/pod_estimation_server.R", local = T)
  source("server/data_explorer_server.R", local = T)
  
}

shinyApp(ui, server)