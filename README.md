# interactiveforestplot
A Shiny App that Produce Forest Plots to Visualize Covariate Effects


### Running Information
Make sure to have all required packages installed code to install pacakges from CRAN and or github is below:

```
install.packages("devtools")
devtools::install_github('smouksassi/interactiveforestplot')
interactiveforestplot::run_interactiveforestplot()
```
### Example
An example data is provided.
Your data should have the following column names:
label,mid,lower,upper,covname,paramname
More on this soon

![example plot with the included dataset](./inst/shiny/img/snapshotforest.png)
