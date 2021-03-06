## ToxPOD 
### Estimating Point of Departure (POD) from dose-response data using spline meta-regression

This app uses the [GRAVEE](https://github.com/k-t-to/gravee) method for estimating point-of-departure from dose-response data. Doses are transformed to log<sub>10</sub> scale prior to analysis. Dose-response data are bootstrapped and fit to an interpolated-spline model. Menger curvature is calculated at points along the curve, with the point-of-departure defined as the dose with the maximal curvature. 

The source code for this Shiny app is available on [GitHub](https://github.com/k-t-to/ToxPOD).

### Launching the app 

ToxPOD can be accessed at  [https://raptorpharmtox.shinyapps.io/ToxPOD/](https://raptorpharmtox.shinyapps.io/ToxPOD/). 


Alternatively, to launch from R, use the following: 

```R
if (!require("shiny")) install.packages("shiny"); library("shiny")
runGitHub("ToxPOD", "k-t-to", subdir = "bin")
```

### Input Data   

Select the method for loading data into the model. User data can be loaded from file or pasted into the text box. Data entries should be delimited by white space (i.e. tabs, spaces).  The data should contain only two columns ordered as dose and response. The first row should be the column headers. Analysis requires that the data have at least 4 doses. Each dose should have at least 3 data points (replicates), otherwise they are excluded from the analysis. Doses should **not** be transformed prior to upload. The values must be numeric. 

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

After loading the data, a plot and table of the input data are shown. The doses will be transformed to log<sub>10</sub> scale.  If the input data contain a dose = 0, the 0 dose is converted to be 1/10th the minimum non-zero dose, so that after log transformation, the distance between the 0 dose and the minimum non-zero dose is 1.  

### Analysis

All analyses are performed using log<sub>10</sub>-transformed doses. Results are back-transformed and reported on the original dose scale.  

Select the number of bootstrap samples to perform (default: 1000). Click `Run Analysis`. A graph and table of the estimated POD distribution is shown. Under the Bootstrap Summary tab, a graph summarizing the interpolated spline fits and POD distribution is shown. Click the `Download Results` botton to download the graphs and results tables.

**Note:** PODs and interpolated doses less than the minimum non-zero dose are rescaled to be left-bound by 0. Thus, in the downloaded results, for PODs and interpolated doses less than the minimum non-zero dose, values in the `log10` columns will not be equal to log<sub>10</sub>(`dose`). 

### Sample Explorer

After running the main analysis, the Sample Explorer allows the user to visualize the bootstrap resamples. Users may manually select the samples to plot, or have a random selection auto-generated. Alternatively, users may choose to summarize all bootstrap samples.  

Click on `Draw Individual Plots` to display graphs showing the fit spline for the selected bootstrap samples and the menger curvatures calculated along the points of the spline curves. Individual plots are not available if all bootstrap samples are selected. 

Click on `Draw Summary Plots` to display two line graphs under the Summary Plots tab. The first graph shows the the simulated dose-response values for the selected bootstrap samples. The second graph shows how the interpolated spline was fit to the bootstrapped samples. 

### License

Copyright 2021 Kim To and Lyle D. Burgoon

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

### Issues/Bugs/Feature Requests

If you identify issues/bugs, or have feature requests, please use the [Issues Page](https://github.com/k-t-to/ToxPOD/issues) at the project's GitHub site.

### Contact

Kim T. To - [kto@ils-inc.com](kto@ils-inc.com)

Lyle Burgoon - [burgoon.lyle@raptorpharmtox.com](burgoon.lyle@raptorpharmtox.com)
