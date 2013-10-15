# KOIs cumulative and active - q1-q16.
# Available at http://exoplanetarchive.ipac.caltech.edu/cgi-bin/ExoTables/nph-exotbls?dataset=cumulative
# and http://exoplanetarchive.ipac.caltech.edu/cgi-bin/nstedAPI/nph-nstedAPI?table=cumulative

# See field descriptions at http://exoplanetarchive.ipac.caltech.edu/docs/API_kepcandidate_columns.html

# Parameters
project_dir <- "C:/projects/NCSU/CSC522/kepler"
koi_url <- "http://exoplanetarchive.ipac.caltech.edu/cgi-bin/nstedAPI/nph-nstedAPI?table=cumulative"
koi_filename <- "koi_cumulative_active_q1_16.csv"

# Package Install
#install.packages("GGally")
#install.packages("lattice")
#install.packages("ggplot2")
# R will check 32-bit builds on a 64-bit architecture, which causes problems.
#install.packages("FSelector")
#install.packages("rJava")



# Setup
# Creates project directory if it does not exist
dir.create(file.path(project_dir), showWarnings = FALSE)
setwd(file.path(project_dir))
require(lattice)
require(ggplot2)
require(GGally)
library(rJava)
require(FSelector)
# If you receive the error:
# error: unable to load shared object 'C:/Program Files/R/R-3.0.1/library/rJava/libs/i386/rJava.dll':
# LoadLibrary failure:  The specified module could not be found.
#
# 1. Make sure that your R and Java installation architectures (x64, i386) match
# 2. Add jvm.dll to your PATH (http://stackoverflow.com/questions/7019912/using-the-rjava-package-on-win7-64-bit-with-r)


# Data download
# Downloads data if it does not exist in project directory
if (!file.exists(koi_filename)) {
  download.file(koi_url, koi_filename)
}
kois <- read.csv(koi_filename)

# Reduce data set to false positive and confirmed planets
false_positives <- kois[kois$koi_disposition=="FALSE POSITIVE",]
confirmed_planets <- kois[kois$koi_disposition=="CONFIRMED",]
candidate_planets <- kois[kois$koi_disposition=="CANDIDATE",]
disposed <- kois[kois$koi_disposition!="CANDIDATE",]

covariates <- names(kois)



pairs(iris[1:4], main = "Edgar Anderson's Iris Data", pch = 21, bg = c("red", "green3", "blue")[unclass(iris$Species)])
#c("red", "green3", "blue")[unclass(iris$Species)]
#c("red", "green3","blue")[unclass(disposed$koi_disposition)]
unclass(disposed$koi_disposition)

unique(disposed$koi_disposition)
pairs(~koi_ror+koi_incl+koi_prad+koi_sage, data=disposed, pch = 21, cex = .6, labels=c("Planet-Star Radius Ratio", "Inclination", "Planetary Radius", "Stellar Age"), main="Scatterplot Matrix", bg = c("white", "green","red")[unclass(disposed$koi_disposition)])
dplotmatrix(iris[,1:4], colour="gray20") + geom_smooth(method="lm")
ggpairs(iris, colour='Species', alpha=0.4)


ggpairs(head(disposed[2:6], 4), colour='koi_disposition', alpha=0.4)


data(iris)
weights <- information.gain(Species~., iris)
print(weights)
subset <- cutoff.k(weights, 2)
f <- as.simple.formula(subset, "Species")
print(f)


