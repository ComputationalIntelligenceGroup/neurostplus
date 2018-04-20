# What 
`neurostrplus` allows you to easily compute a number of morphometrics. It build upon branch- and node-level metrics computed by NeuroSTR (via package `neurostr`). In addition, it includes a number of morphometrics specifically designed for GABAergic interneurons. See the **vignette** "" for the definition of these morphometrics.  

# Install  
`neurostrplus` requires `neurostr` to be installed. The following installs both packages (install the `devtools` package with `install.package('devtools')` if you don't have it):

```{r, eval = FALSE}
# install.package('devtools')
devtools::install_github("ComputationalIntelligenceGroup/neurostrr")
devtools::install_github("ComputationalIntelligenceGroup/neurostrplus")
```         
