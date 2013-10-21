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
disposed <- subset(kois, !kepoi_name %in% candidate_planets$kepoi_name)

# drop factor levels
false_positives$koi_disposition <- factor(false_positives$koi_disposition)
confirmed_planets$koi_disposition <- factor(confirmed_planets$koi_disposition)
candidate_planets$koi_disposition <- factor(candidate_planets$koi_disposition)
disposed$koi_disposition <- factor(disposed$koi_disposition)

# useful covariates; removed koi_longp, koi_smet, koi_pdisposition
covariates <- c("ra_str", "dec_str", "koi_period", "koi_time0bk", "koi_depth", "koi_duration", "koi_ingress", "koi_impact", "koi_incl", "koi_sma", "koi_eccen", "koi_ror", "koi_dor", "koi_prad", "koi_teq", "koi_steff", "koi_slogg", "koi_srad", "koi_smass", "koi_sage", "koi_sparprov", "koi_vet_date")

pairs(~koi_ror+koi_incl+koi_prad+koi_sage, data=disposed, pch = 21, cex = .6, labels=c("Planet-Star Radius Ratio", "Inclination", "Planetary Radius", "Stellar Age"), main="Scatterplot Matrix", bg = c("white", "green","red")[unclass(disposed$koi_disposition)])
pairs(~dec_str+ra_str+koi_ingress+koi_sparprov+koi_dor, data=disposed, pch = 21, cex=.6, bg = c("white", "green","red")[unclass(disposed$koi_disposition)])
# Some other pairwise plot options
plotmatrix(iris[,1:4], colour="gray20") + geom_smooth(method="lm")
#plotmatrix(~dec_str+ra_str+koi_ingress+koi_sparprov+koi_dor, data=head(disposed, 10), colour="gray20") + geom_smooth(method="lm")

#ggpairs(iris, colour='Species', alpha=0.4)
#ggpairs(head(disposed[2:6], 4), colour='koi_disposition', alpha=0.4)

ggplot(disposed, aes(x=ra_str, y=dec_str, color=koi_disposition)) + geom_point(shape=1) + scale_y_discrete(breaks=seq(0, 10, 0.25)) + theme(axis.ticks=element_blank(), axis.text.y = element_blank())
p <- ggplot(disposed, aes(factor(koi_disposition), koi_incl))
p <- p + geom_boxplot(outlier.colour="#2F4F4F", fill="#CDCDCD", colour="#2F4F4F") + coord_flip()
p <- p + theme(axis.title.y = element_blank()) + ylab("Inclination (degrees)")
p

p <- ggplot(disposed, aes(factor(koi_disposition), koi_prad))
p + geom_boxplot(outlier.colour="#2F4F4F", fill="#CDCDCD", colour="#2F4F4F") + coord_flip()

p <- ggplot(disposed, aes(factor(koi_disposition), koi_ror))
p + geom_boxplot(outlier.colour="#2F4F4F", fill="#CDCDCD", colour="#2F4F4F") + coord_flip()

p <- ggplot(disposed, aes(factor(koi_disposition), koi_sage))
p + geom_boxplot(outlier.colour="#2F4F4F", fill="#CDCDCD", colour="#2F4F4F") + coord_flip()

p <- ggplot(disposed, aes(factor(koi_disposition), koi_ingress))
p + geom_boxplot(outlier.colour="#2F4F4F", fill="#CDCDCD", colour="#2F4F4F") + coord_flip()

p <- ggplot(disposed, aes(factor(koi_disposition), koi_dor))
p + geom_boxplot(outlier.colour="#2F4F4F", fill="#CDCDCD", colour="#2F4F4F") + coord_flip()



weights <- information.gain(koi_disposition~., disposed[, names(disposed) %in% c(covariates, "koi_disposition")])
weights[ order(-weights[,1]), ,drop=FALSE ]
# higher attr_importance is better
attribute_subset <- cutoff.k(weights, 5)
f <- as.simple.formula(attribute_subset, "koi_disposition")
print(f)



### Notes
# Debug commands: traceback(), options(error=recover)
