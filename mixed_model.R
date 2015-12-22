#load library
library(lme4)
#Read data
plants=read.csv('Final Project_Part 2_plantgrowth.csv')
plants$week=factor(plants$week)
as.numeric.factor <- function(x) {seq_along(levels(x))[x]}
plants$plant=as.numeric.factor(plants$plant)
str(plants )

#fit a mixed model
#assuming the data is normally distributed
m.model <- lmer(plant ~ treatment + week+(week*treatment)+(1|week), data = plants)
#summary
summary(m.model)
#use anova
anova(m.model)
#plot residuals vs fitted
plot(m.model)
residuals=residuals(m.model)
fitted=fitted(m.model)
plot(residuals,fitted)
#the lineality assumption is met
#strip plot
stripchart(residuals(m.model) ~ plants$treatment, vertical = TRUE,  pch=6, method = "jitter",col=c("red","blue","orange"))
#scatter plot
library(car)
scatterplot(plants$week~residuals(m.model))
#scatter plot grouped by treatment
plot(residuals,plants$week,pch=5,bg=c("red","green3","blue")[unclass(plants$treatment)], 
     main="scatter plot grouped by treatment")
#add linea regression line to the plots
lm=lm(plant ~ treatment + week+(week*treatment),data=plants)
#reiduals vs fitted
plot(residuals,fitted)
abline(lm)
#strip chart
stripchart(residuals(m.model) ~ plants$treatment, vertical = TRUE,  pch=6, method = "jitter",col=c("red","blue","orange"))
abline(lm)
# scatter plot
scatterplot(plants$week~residuals(m.model))
abline(lm)
#grouped scatter plot
plot(residuals,plants$week,pch=5,bg=c("red","green3","blue")[unclass(plants$treatment)], 
     main="scatter plot grouped by treatment")
abline(lm)




