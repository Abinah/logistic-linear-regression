#pcae
#PART FOUR
library(CCA)
library(corrplot)
library(psych)
#EXAMINE ASSOCIATION AMONG VARIABLES
final=data=read.csv("Final Project_Part 3_planttype.csv")
final2=final[,-9]
summary(final2)
#a scatter plot of the data
pairs(final2)
#cerrelation matrix to examine association between variablea
matcor(final2,final2)
#visualize the correlation matrix
m=cor(final2)#use cor() since corrplot() takea a matrix
corrplot(m,method="circle")

#Carry out a principal components analysis on the plant measurements, x1-x8
#principal component analysis
pca=prcomp(final2)

#Examine the proportion of variance explained by each principal component.
#variance explained by each component
s=summary(pca)
s
print("PC1 EXPLAINS 39% OF VARIANCE")
print("PC2 EXPLAINS 22% OF VARIANCE")
print("PC3 EXPLAINS 15% OF VARIANCE")
print("PC4 EXPLAINS 10% OF VARIANCE")
print("PC5 EXPLAINS 7% OF VARIANCE")
print("PC6 EXPLAINS 5% OF VARIANCE")
print("PC7 EXPLAINS 2% OF VARIANCE")
print("PC8 EXPLAINS 1% OF VARIANCE")

#Create a scree plot to visualize the magnitudes of the eigenvalues
#scree plot
VSS.scree(cor(final2))

#Create a biplot to visualize the contribution of variables to the first two principal components
#biplot
biplot(pca)


#eigen vectors for the first two components
eigen_vectors1=pca$rotation[,1:2]

#eigen vectors for the first 3rd,4th and 5th components
eigen_vectors2=pca$rotation[,3:5]

#Save the scores for the first two principal components and create a scatterplot using them
#scores
scrs1=as.data.frame(pca$x[,1:2])
plot(scrs1,pch=final$type,ylim=c(-2,2),xlim=c(-2,2),xlab="PCA1",ylab="PCA2")
legend(x="topright",legend=levels(final$type),pch=final$type)


#DOES PCA HELP
print("yes it does help by showing the main variable that can be used to predict the dependent variable")
