## ToxPOD 
### Estimating Point of Departure (POD) from dose-response data using spline meta-regression

This app uses the [GRAVEE](https://github.com/k-t-to/gravee) method for estimating point-of-departure from dose-response data. Doses are transformed to log<sub>10</sub> scale prior to analysis. Dose-response data are bootstrapped and fit to an interpolated-spline model. Menger curvature is calculated at points along the curve, with the point-of-departure defined as the dose with the maximal curvature. 

The source code for this Shiny app is available on [GitHub](https://github.com/k-t-to/gravee_app).

### Launching the app 

From R, use the following to launch the app: 

```R
if (!require("shiny")) install.packages("shiny"); library("shiny")
runGitHub("ToxPOD", "k-t-to", subdir = "bin")
```

### Input Data  

Upload data as a tab-delimited text file. The data should contain only two columns ordered as dose and response. Doses should **not** be transformed prior to upload. The values must be numeric. 

**Example:**

| Dose  | Response |
| --------- | --------- |
| 1  | 1.4  |
| 1  | 0.28  |
| 1  | 0.9  |
| 5  | 8.32  |
| 5  | 8.6  |
| 5  | 7  |
| 10  | 10.2  |
| 10  | 9.45  |
|...|...|

After uploading a data file, a plot and table of the input data are shown. The doses will be transformed to log<sub>10</sub> scale.

### Analysis

All analyses are performed using log<sub>10</sub>-transformed doses. Results are reported on both the original and log<sub>10</sub> scale.

Select the number of bootstrap samples to perform. The default is 500. Click `Run Analysis`. Under the results tab, a distribution of the estimated PODs is shown. Click the Download Results button to download the POD estimates, Menger Curvature calculations, and the POD distribution graphs.

### Sample Explorer

After running the main analysis, the Sample Explorer allows the user to visualize the bootstrap resamples.

The Bootstrap Summary tab will display a summary of the spline-interpolation. The summary plot shows the median predicted response at each dose and the minimum and maximum predicted responses across all bootstraps for each dose. A histogram at the bottom shows how the estimated PODs are distributed across the interpolated doses.

Under `Bootstrap Samples`, select the numeric IDs of bootstrap samples to plot. `Draw Plots` will display individual graphs of the curves for the selected samples.

### License

This work was created by US government employees working in their government capacity. As a result, this work is in the public domain within the United States. The US Government may exercise copyright in other jurisdictions.

### Contact

Kim T. To - [kimberly.t.to@erdc.dren.mil](kimberly.t.to@erdc.dren.mil)

Lyle Burgoon - [lyle.d.burgoon@erdc.dren.mil](lyle.d.burgoon@erdc.dren.mil)
