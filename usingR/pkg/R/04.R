education <- read.csv("http://datasets.flowingdata.com/education.csv", header = TRUE)
ed.dis <- dist(education[,2:7])
ed.mds <- cmdscale(ed.dis)
x <- ed.mds[,1]
y <- ed.mds[,2]
p <- plot(x,y, type="n")
text(x,y,labels=education$state)
install.packages("mclust")
library(mclust)
ed.mclust <- Mclust(ed.mds)
plot(ed.mclust, data=ed.mds)