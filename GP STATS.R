#geoffrey assignment

library(readr)
library(tidyr)

#data was manually imported, and the format was changed to the wide format
require(reshape2)
PAQ_Suvi_Wide=dcast(PAQ_Suvi, id ~ var, value.var = "value")
view(PAQ_Suvi_Wide)

summary(PAQ_Suvi_Wide)


#participant 158 left Q1 empty. This participant is removed

Wide_Data_clean <- PAQ_Suvi_Wide[!(PAQ_Suvi_Wide$id==158),]
summary(Wide_Data_clean)

#PCA



data_pcacov = princomp(Wide_Data_clean, cor=FALSE) 
summary(data_pcacov, loadings=TRUE) 


PAQ_pca <- princomp(Wide_Data_clean[,-1], cor = TRUE)
summary(PAQ_pca)


biplot(PAQ_pca, col = c("pink", "black")) 

plot(data_pcacor$sdev^2, xlab = "Component number",
     ylab = "Component variance", 
     type = "l", 
     main = "Scree diagram")

##Log(eigenvalue) diagram
plot(log(data_pcacor$sdev^2), 
     xlab = "Component number", 
     ylab = "log(Component variance)", 
     type="l", 
     main = "Log(eigenvalue) diagram")

# Correlation matrix! Sd over 1 is bad
#antani
#part 2

#loading data manually

view(Nations)

# loading necessary packages
library(MASS)
library(smacof)
library(psych)
library(vegan)
library(factoextra)

#multidimensional scaling
#stress is 2

require(smacof) 
nationsdiss = sim2diss(Nations, method = 7, to.dist = TRUE)

require(MASS) 
nationsE_mds <- isoMDS(nationsdiss)

nationsE_mds$stress

#stressplot
stressplot(nationsE_mds, nationsdiss)

#Shepard's plot

nations.x <- as.matrix(Nations[, -1])
nations.dist <- dist(nations.x)
nations.mds <- isoMDS(nations.dist)
plot(nations.mds$points, type = "n")
text(nations.mds$points, labels = as.character(1:nrow(swiss.x)))
nations.sh <- Shepard(nations.dist, nations.mds$points)
plot(nations.sh, pch = ".")
lines(nations.sh$x, nations.sh$yf, type = "S")

#Two dimensional 


x <- nationsE_mds$points[,1] 
y <- nationsE_mds$points[,2] 
plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2", xlim = range(x)*1.2, type = "n", 
     main = "Two-dimensional solution from non-metric multidimensional scaling") +
  text(x, y, labels = colnames(Nations), col="pink")




