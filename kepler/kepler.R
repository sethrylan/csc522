# KOIs cumulative and active - q1-q16.
# Available at http://exoplanetarchive.ipac.caltech.edu/cgi-bin/ExoTables/nph-exotbls?dataset=cumulative
# and http://exoplanetarchive.ipac.caltech.edu/cgi-bin/nstedAPI/nph-nstedAPI?table=cumulative

# See field descriptions at http://exoplanetarchive.ipac.caltech.edu/docs/API_kepcandidate_columns.html

# Parameters
project_dir <- "/Users/gaineys/projects/NCSU/CSC522/kepler"
koi_url <- "http://exoplanetarchive.ipac.caltech.edu/cgi-bin/nstedAPI/nph-nstedAPI?table=cumulative"
koi_filename <- "koi_cumulative_active_q1_16.csv"

exoplanets_url <- "http://exoplanetarchive.ipac.caltech.edu/cgi-bin/nstedAPI/nph-nstedAPI?table=exoplanets&select=pl_hostname,pl_discmethod,pl_orbper,pl_orbsmax,pl_orbeccen,pl_massj,pl_msinij,pl_radj,pl_dens,pl_orbincl,pl_ttvflag,ra,dec,st_dist,st_vj,st_teff,st_mass,pl_publ_date"
exoplanets_filename <- "exoplanets.csv"


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
library(rpart)
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

if (!file.exists(exoplanets_filename)) {
  download.file(exoplanets_url, exoplanets_filename)
}
exoplanets <- read.csv(exoplanets_filename)

# Find new planets
#sorted <- exoplanets[with(exoplanets, order(pl_publ_date )), ]
#sorted[1:1000, c("pl_name", "pl_publ_date")]

# Reduce data set to false positive and confirmed planets
false_positives <- kois[kois$koi_disposition=="FALSE POSITIVE",]
confirmed_planets <- kois[kois$koi_disposition=="CONFIRMED",]
candidate_planets <- kois[kois$koi_disposition=="CANDIDATE",]
disposed <- subset(kois, !kepoi_name %in% candidate_planets$kepoi_name)

# A 14:1 class size ratio
table(factor(disposed$koi_disposition))


# useful attributes; removed koi_longp, koi_smet, koi_pdisposition
covariates <- c("ra_str", "dec_str", "koi_period", "koi_time0bk", "koi_depth", "koi_duration", "koi_ingress", 
                "koi_impact", "koi_incl", "koi_sma", "koi_eccen", "koi_ror", "koi_dor", "koi_prad", "koi_teq", 
                "koi_steff", "koi_slogg", "koi_srad", "koi_smass", "koi_sage", "koi_sparprov", "koi_vet_date")

subset <- disposed[, names(disposed) %in% c(covariates, "koi_disposition")]
write.table(subset, file = paste(koi_filename,".preprocessed.csv"), sep= ",")

# drop factor levels
false_positives$koi_disposition <- factor(false_positives$koi_disposition)
confirmed_planets$koi_disposition <- factor(confirmed_planets$koi_disposition)
candidate_planets$koi_disposition <- factor(candidate_planets$koi_disposition)
disposed$koi_disposition <- factor(disposed$koi_disposition)


pairs(~koi_ror+koi_incl+koi_prad+koi_sage, data=disposed, pch = 21, cex = .6, 
      labels=c("Planet-Star Radius Ratio", "Inclination", "Planetary Radius", "Stellar Age"), 
      main="Scatterplot Matrix", bg = c("white", "green","red")[unclass(disposed$koi_disposition)])
pairs(~dec_str+ra_str+koi_ingress+koi_sparprov+koi_dor, data=disposed, pch = 21, cex=.6, bg = c("white", "green","red")[unclass(disposed$koi_disposition)])
# Some other pairwise plot options
plotmatrix(subset[,c(5,6,11,14,15)], colour="gray20") + geom_smooth(method="lm")
plotmatrix(subset, colour="gray20") + geom_smooth(method="lm")
#plotmatrix(~dec_str+ra_str+koi_ingress+koi_sparprov+koi_dor, data=head(disposed, 10), colour="gray20") + geom_smooth(method="lm")
#ggpairs(iris, colour='Species', alpha=0.4)
#ggpairs(head(disposed[2:6], 4), colour='koi_disposition', alpha=0.4)


# Scatterplot of RA and DEC angles for confirmed and FP classes
ggplot(disposed, aes(x=ra_str, y=dec_str, color=koi_disposition)) + geom_point(shape=16) + scale_y_discrete(breaks=seq(0, 10, 0.25)) + theme(axis.ticks=element_blank(), axis.text.y = element_blank())

# Boxplot of inclination for confirmed and FP classes
p <- ggplot(disposed, aes(factor(koi_disposition), koi_incl))
p <- p + geom_boxplot(outlier.colour="#2F4F4F", fill="#CDCDCD", colour="#2F4F4F") + coord_flip()
p <- p + theme(axis.title.y = element_blank()) + ylab("Inclination (degrees)")
p

# Boxplot of planetary radius for confirmed and FP classes
p <- ggplot(disposed, aes(factor(koi_disposition), koi_prad))
p <- p + geom_boxplot(outlier.colour="#2F4F4F", fill="#CDCDCD", colour="#2F4F4F") + coord_flip()
p <- p + theme(axis.title.y = element_blank()) + ylab("Planetary Radius (Earth Radii)")
p

# Boxplot of Planet-Start Radius Ratio (RoR) for confirmed and FP classes
p <- ggplot(disposed, aes(factor(koi_disposition), koi_ror))
p <- p + geom_boxplot(outlier.colour="#2F4F4F", fill="#CDCDCD", colour="#2F4F4F") + coord_flip()
p <- p + theme(axis.title.y = element_blank()) + ylab("Planet-Star Radius Ratio")
p

p <- ggplot(disposed, aes(factor(koi_disposition), koi_incl))
p <- p + geom_boxplot(outlier.colour="#2F4F4F", fill="#CDCDCD", colour="#2F4F4F") + coord_flip()
p <- p + theme(axis.title.y = element_blank()) + ylab("Inclination (degrees)")
p


# Population Differences between False Positives/Confirmed Planets by Planet-Star Radius Ratio (RoR) and Inclination Angle (degrees)
ggplot(disposed, aes(x=koi_incl, y=koi_ror, color=koi_disposition)) + geom_point(shape=1, alpha=.5) + ylab("Planet-Star Radius Ratio") +  xlab("Inclination (degrees)")

# TODO: http://cran.r-project.org/doc/contrib/Zhao_R_and_data_mining.pdf

useful_covariates <- c("koi_steff", "koi_smass", "koi_sage", "koi_time0bk", "koi_slogg", "koi_incl", "koi_srad", "koi_dor", "koi_sma", "koi_ror")
evaluator <- function(subset) {
  #k-fold cross validation
  k <- 5
  splits <- runif(nrow(disposed))
  results = sapply(1:k, function(i) {
    test.idx <- (splits >= (i - 1) / k) & (splits < i / k)
    train.idx <- !test.idx
    test <- disposed[test.idx, , drop=FALSE]
    train <- disposed[train.idx, , drop=FALSE]
    tree <- rpart(as.simple.formula(subset, "koi_disposition"), train)
    error.rate = sum(test$koi_disposition != predict(tree, test, type="c")) / nrow(test)
    return(1 - error.rate)
  })
  print(subset)
  print(mean(results))
  return(mean(results))
}
subset <- exhaustive.search(useful_covariates, evaluator)
f <- as.simple.formula(subset, "koi_disposition")
print(f)


# SVM Example
require(e1071)
m <- svm(koi_disposition~koi_incl+koi_sma+koi_ror+koi_dor+koi_srad, data = disposed)
plot(m, disposed)
m
m$SV
m$index
m$coefs


# AdaBoost
pruned <- read.csv("koi_cumulative_active_q1_16.csv.pruned.csv", sep=",", na.strings = "NA")
pruned$koi_disposition <- gsub("\\'FALSE POSITIVE\\'", "F", pruned$koi_disposition)
pruned$koi_disposition <- gsub("CONFIRMED", "C", pruned$koi_disposition)
require(ada)
set.seed(42)
##set up testing and training data (60% for training)
n<-dim(pruned)[1]
trind<-sample(1:n,floor(.6*n),FALSE)
teind<-setdiff(1:n,trind)
##fit 8-split trees
gdis<-ada(koi_disposition~koi_incl+koi_sma+koi_ror+koi_dor+koi_srad,data=pruned[trind,],iter=20,nu=1,type="discrete")
##add testing data set
gdis=addtest(gdis,pruned[teind,-1], pruned[teind,1], na.action="na.exclude")
##plot gdis
plot(gdis,TRUE,TRUE)
##variable selection plot
varplot(gdis)
##pairwise plot
pairs(gdis,pruned[trind,-1],maxvar=5)



# Missing values for inclination:
sapply(pruned[pruned$koi_disposition=='F',], function(x) sum(is.na(x)))
sapply(pruned[pruned$koi_disposition=='C',], function(x) sum(is.na(x)))



# Calculate information gain for our attributes:
# Where RatioGAIN = GainINFO/H(Attribute) = (H(Class) + H(Attribute) - H(Class|Atribute))/H(Attribute)
# See FSelector docs: http://cran.r-project.org/web/packages/FSelector/FSelector.pdf
weights <- gain.ratio(koi_disposition~., disposed[, names(disposed) %in% c(covariates, "koi_disposition")])
weights <- gain.ratio(koi_disposition~., disposed)
weights[ order(-weights[,1]), ,drop=FALSE ]
# higher attr_importance is better
attribute_subset <- cutoff.k(weights, 5)
f <- as.simple.formula(attribute_subset, "koi_disposition")
print(f)


# Trends for discovery method
p <- ggplot(exoplanets, aes(factor(pl_discmethod), pl_massj))
p <- p + geom_boxplot(outlier.colour="#2F4F4F", fill="#CDCDCD", colour="#2F4F4F") + coord_flip()
p <- p + theme(axis.title.y = element_blank()) + ylab("Planetary Mass (Jupiter)")
p

p <- ggplot(exoplanets, aes(factor(pl_discmethod), pl_radj))
p <- p + geom_boxplot(outlier.colour="#2F4F4F", fill="#CDCDCD", colour="#2F4F4F") + coord_flip()
p <- p + theme(axis.title.y = element_blank()) + ylab("Planetary Radius (Jupiter)")
p

# Outliers removed in this plot
p <- ggplot(exoplanets[exoplanets$pl_orbper < 20000,], aes(factor(pl_discmethod), pl_orbper))
p <- p + geom_boxplot(outlier.colour="#2F4F4F", fill="#CDCDCD", colour="#2F4F4F") + coord_flip()
p <- p + theme(axis.title.y = element_blank()) + ylab("Planetary Orbital Period")
p






pruned <- read.csv("koi_cumulative_active_q1_16.csv.pruned.csv", sep=",", na.strings = "NA")
pruned$koi_disposition <- gsub("\\'FALSE POSITIVE\\'", "F", pruned$koi_disposition)
pruned$koi_disposition <- gsub("CONFIRMED", "C", pruned$koi_disposition)
disposed.tree <- rpart(koi_disposition ~ ., data=pruned, method="class", parms=list(split='information'), control=rpart.control(minsplit=1,minbucket=1, cp=0))
plot(disposed.tree, margin = .04, uniform=TRUE,   main="Unpruned Classification Tree for Entropy")
text(disposed.tree, use.n=TRUE, all=TRUE, cex=.5)
printcp(disposed.tree)



library(class)
library(mice)
library(mda)
pruned.imp <- mice(pruned)
pruned.complete <- complete(pruned.imp)
steps = seq(1, 8)
nas = rep(NA, length(steps))
confusions <- data.frame(k=nas, error=nas, stringsAsFactors=FALSE)
for(k in steps) {
  pruned.knn <- knn(train=pruned.complete[,-1], test=pruned.complete[,-1], cl=pruned.complete$koi_disposition, k=k)
  cf <- confusion(object=pruned.knn, true=pruned.complete$koi_disposition)
  confusions[k,] <- c(k,attr(cf, "error"))
}
confusions
ggplot(data=confusions, aes(x=k, y=error)) + geom_line() + geom_point() + ggtitle("kNN Resubstitution Error Rate")


require(VIM)
require(mice)

barMiss(exoplanets, delimiter="_pl_discmethod", pos=9)


traceback()


### Notes
# Debug commands: traceback(), options(error=recover)
