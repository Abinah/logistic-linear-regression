#LDA
#PART THREE
#Carry out a linear discriminant analysis to find the variable combination of x1-x8 that best discriminates plants of different types
library(MASS)
data=read.csv("Final Project_Part 3_planttype.csv")
data$type=factor(data$type)
ld=lda(type~x1+x2+x3+x4+x5+x6+x7+x8,data=data)
#draw a histogram
lds=predict(ld,data)
ldahist(data = lds$x[,1],g=type,xlab = lds$class)
#reclassify
ldre=predict(ld,data[,1:8])
table(ldre$class,data$type)
#Which model performs better, the logistic regression model or the linear discriminant analysis
print("the linear discriminant model works better")