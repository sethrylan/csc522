Exercises from *Introduction to Probability and Statistics Using R (2ed)* by G. Jay Kerns 
======

## Requirements for building ##
 * Installation of R (see http://cran.rstudio.com/bin/windows/base/README.R-3.0.1)
 * Add <R_HOME>/bin/<arch> to path
 * ```Rscript -e "options('repos'='http://cran.us.r-project.org'); install.packages('roxygen2', dep = TRUE)"```
 
 * install.packages('IPSUR', dep = TRUE)
 * install.packages('Rcmdr', dep = TRUE)
 * install.packages('XLConnect', dep = TRUE)
 * install.packages('RcmdrPlugin.IPSUR', dep = TRUE)


 
 
## Notes ##
http://stackoverflow.com/questions/3412911/r-exe-rcmd-exe-rscript-exe-and-rterm-exe-whats-the-difference


## .Rprofile ##
    options("width"=160)
    #options("pdfviewer"="kpdf")
    # as in AER by Zeileis and Kleiber
    options(prompt="R> ", digits=4, show.signif.stars=FALSE)
    options("repos"="http://cran.us.r-project.org")    ## US mirror
