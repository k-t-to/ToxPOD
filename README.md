### Good Risk Assessment Values for Environmental Exposures
#### Estimating Point of Departure (POD) from dose-response data using spline meta-regression

This app uses the GRAVEE method for estimating point of departure from dose-response data. 

#### Launching the app 

From R, use the following to launch the app: 

```R
if (!require("shiny")) install.packages("shiny"); library("shiny")
runGitHub("GRAVEE_App", "k-t-to", subdir = "bin")
```

#### Input Data  

The input data should be a tab-delimited .txt file containing dose-response data for a single assay. The data should have two columns for the Dose and Response. 
