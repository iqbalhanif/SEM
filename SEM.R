#read data
data=read.csv("D:/DATA/Desktop/Marketing.csv", sep=";")

#library
library(lavaan)
library(semPlot)
library(MVN)
library(semTools)

#modelling
model <- '
# measurement model
LK =~ Y1 + Y2 + Y3
KL =~ X1 + X2 + X3
CM =~ X4 + X5 + X6 + X7
PP =~ CM + KL
# Structural model
LK ~ PP
'
fit <- sem(model, data=data)

#Path Diagram
semPaths(fit,whatLabels="std", style="lisrel", layout="tree2", exoCov = FALSE, 
         sizeMan=5,sizeLat=5, sizeMan2=5, sizeLat2=5,edge.label.cex=0.8, 
         groups = "latents", pastel=T)

#Standardized
standardizedSolution(fit)  #Valid
reliability(fit) #reliabel
parameterEstimates(fit) #parameter
summary(fit,rsquare=TRUE) #summary model (rsquare)
fitMeasures(fit) #goodness of fit model

#Normal
normal=mvn(data)
normal$Descriptives
normal$univariateNormality
normal$multivariateNormality