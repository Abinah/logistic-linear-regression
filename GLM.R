#part 1
#read data

final=read.csv("Final Project_Part 3_planttype.csv")
#use str
str(final)
#head
head(final)
#scarterplot matrix
attach(final)
pairs(final[,1:8],col=c(x1,x2,x3,x4,x5,x6,x7,x8))


#PART TWO
#FIT logit regression modell
lfit=glm(type~x1+x2+x4+x5+x6+x7+x8,family=binomial,data=final)
#Add a fitted logistic regression probability curve to a plot of plant type versus predicted values 
predicted=predict(lfit,final,type="response")
plot(final$type,predicted)
points(type,fitted(lfit),pch=20)
library(ggplot2)
ggplot(final, aes(x=final$type, y=predicted)) + geom_point() + 
    stat_smooth(method="glm", family="binomial", se=FALSE)

# USE summary COMMAND
summary(lfit)

#use the coefficient to calculate predicted value
est=round(12.3-(6*1.2)+(1.4*7)-(0.8*7)-(0.6*6)-(0.7*6)-(1*6)+(0.8*7),5)

#Using the inverse of the logit transformation, convert this prediction to a probability of plant type 1 on the original scale
library(boot)
inv.logit(est)

#Calculate the likelihood-based 95% confidence interval for the logistic regression coefficients.
exp(confint(lfit,level=0.95))


#reclassify
round(predict(lfit,final,type="response"),5)
predict <- ifelse(predict(lfit, type="response")>.5, 1, 0)
xtabs(~predict+final$type)
