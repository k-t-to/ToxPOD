################################
# UI for About Page
################################

# Heading
about_header <- column(8,
                       h3("Good Risk Assessment Values for Environmental Exposures"),
                       h4("Estimating Point of Departure (POD) from dose-response data using spline meta-regression"),
                       br(),
                       p("This app uses the GRAVEE method for estimating point-of-departure from dose-response data. 
                          Dose-response data are bootstrap and fit to an interpolated-spline model. Menger curvature 
                          is calculated at points along the curve, with the point-of-departure defined as the
                          dose with the maximal curvature."),
                       br(),
                       p("The source code for this Shiny app is available on ",
                         a("github", href = "https://github.com/k-t-to/gravee_app"), "."),
                       offset = 2)

# Instructions
instructions_section <- column(8,
                              h4("Instructions"),
                              h5("Input Data"),
                              p("Upload data as a tab-delimited text file. The data should contain only two columns 
                                 ordered as dose and response. The values must be numeric. For more examples, view 
                                 the example files on the project's github."),
                              h6("Example: "),
                              offset = 2)
in_example_section <- column(4,
                             dataTableOutput("about_in_example"),
                             align = "center",
                             offset = 4)

analysis_section <- column(8,
                           br(),
                           p("After uploading a data file, a plot and table of the input data are shown."),
                           h5("Analysis"),
                           p("Select the number of bootstrap samples to perform. The default is 500. Click", code("Run Analysis"),
                             "Under the results tab, a distribution of the estimated PODs is shown.
                             Click the", code("Download Results"), "button to download the POD estimates, Menger
                             Curvature calculations, and the POD distribution graph."),
                           offset = 2)

# Sample Explorer
sample_ex_section <- column(8,
                            h5("Sample Explorer"),
                            p("After running the main analysis, the Sample Explorer allows the user to
                               visualize the bootstrap resamples."),
                            br(),
                            p(code("Draw Summary"), "will display a summary of the spline-interpolation.
                              The summary plot shows the median predicted response at each dose and the
                              minimum and maximum predicted responses across all bootstraps for each dose.
                              A histogram at the bottom shows how the estimated PODs are distributed across
                              the interpolated doses."),
                            br(),
                            p("Under Bootstrap Samples, select the numeric IDs of bootstrap samples
                               to plot.", code("Draw Plots"), "will display individual graphs of the
                               curves for the selected samples."),
                            offset = 2)

# License
license_section <- column(8,
                          h4("License"),
                          p("This work was created by US government employees working in their government
                             capacity. As a result, this work is in the public domain within the United States. 
                             The US Government may exercise copyright in other jurisdictions."),
                          offset = 2)

# Contact
contact_section <- column(8,
                          h4("Contact"),
                          p(""))