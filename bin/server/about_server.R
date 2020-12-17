################################
# Server for About Page
################################

# Input data example
about_in_example <- data.frame(
  dose = c(0,0,1,1,2,2),
  response = c(0.1,0.5,0.2,1,5,6)
)

# Display input data
output$about_in_example <- renderDataTable(
  datatable(about_in_example,
            rownames = FALSE,
            options = list(dom = "t"))
)
