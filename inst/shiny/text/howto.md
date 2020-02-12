1. Upload your data file in csv format. R default options for `read.csv` will apply 
except for missing values where both (NA) and dot (.) are treated as missing.
`read.csv("yourdata.csv", na.strings = c("NA", "."))`

2. The uploaded data will map automatically it is a must that you have the following column names:
    + paramname: Parameter on which the effects are shown e.g. CL, Cmax, AUC etc.  
    + covname: Covariate name that the effects belong to e.g. Weight, SEX, Dose etc.  
    + label: Covariate value that the effects of which is shown e.g. 50 kg, 50 kg\90 kg (here the reference value is contained in the label).   
mid: Middle value for the effects usually the median from the uncertainty distribution.   
    + lower: Lower value for the effects usually the 2.5% or 5% from the uncertainty distribution.   
    + upper: Upper value for the effects usually the 97.5% or 95% from the uncertainty distribution.

Refer to the repo readme file for additional info:
<a href="https://github.com/smouksassi/coveffectsplot" target="_blank">Click Here to go to the github repo.</a>

The package vignettes include and introduction and multiple detailed examples:  
<a href="https://cran.r-project.org/web/packages/coveffectsplot/vignettes/introduction_to_coveffectsplot.html">Introduction to coveffectsplot.</a>   
<a href="https://cran.r-project.org/web/packages/coveffectsplot/vignettes/PK_Example.html">PK example</a>  
<a href="https://cran.r-project.org/web/packages/coveffectsplot/vignettes/PKPD_Example.html">PK PD example</a>     
<a href="https://cran.r-project.org/web/packages/coveffectsplot/vignettes/Exposure_Response_Example.html">Exposure Response</a>  

Contact me @ samermouksassi@gmail.com for feedback/bugs/features/pull requests!

*Samer Mouksassi 2020*
