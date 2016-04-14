## set working directory
setwd("~/DBS/AdvancedDataAnalytics/Assignments/CA3-Yachts/Submission")

## check working directory
getwd()

## read in dataset
yachts <- read.csv("yachtData.csv", header = TRUE)

## view dataset
View(yachts)
colnames(yachts)
colnames(yachts) <- c("YachtBuilder", "LOA", "Beam", "Draft", "Displacement")

## check
View(yachts)

## check summary
summary(yachts)

## check structure
str(yachts)

## package installation
install.packages("car")

## visualise the data.
pairs(yachts)

## The 1st variable (YachtBuilder) is numeric but only has 4 distinct values,
## (which are clearly visible in the scatterplot).
pairs(yachts[2:5])

## Ok, now look at a scatterplot on steroids:
library(car)
scatterplotMatrix(yachts[2:5])

## examine the arguments for this function,
## change the display for the diagonals.
## The diagonals contain information on a single variable
scatterplotMatrix(yachts[2:5], diagonal="boxplot")
scatterplotMatrix(yachts[2:5], diagonal="histogram")
scatterplotMatrix(yachts[2:5], diagonal="qqplot")

## scale the data & re-plot
scatterplotMatrix(scale(yachts[2:5]), diagonal="boxplot")
scatterplotMatrix(scale(yachts[2:5]), diagonal="histogram")
scatterplotMatrix(scale(yachts[2:5]), diagonal="qqplot")

## Note: re-scaling the data does not fundamentally change the data.
## Don't believe me, well here is the proof:
cor(yachts[2:5])
cor(scale(yachts[2:5]))


## If you see an interesting scatterplot for 2 variables in the matrix scatterplot,
## you may want to plot that scatterplot in more detail,
## with the data points labelled by their group (their YachtBuilder in this case).
## For example, in the matrix scatterplot above, the cell in the 3rd column of the 4th row down
## is a scatterplot of Displacement (x-axis) against Draft (y-axis).
## If you look at this scatterplot, it appears that there may be a positive relationship between Displacement and Draft.
## We may therefore decide to examine the relationship between Displacement and Draft more closely,
## by plotting a scatterplot of these 2 variables, with the data points labelled by their group (YachtBuilder).
## To plot a scatterplot of 2 variables, we can use the "plot" R function.
## The Draft and Displacement variables are stored in the columns 4 & 5 of the dataset "yachts",
## so can be accessed by typing yachts$Draft or yachts$Displacement.
## Therefore, to plot the scatterplot, we type:
plot(yachts$Draft, yachts$Displacement)
#text(yachts$Draft, yachts$Displacement, yachts$YachtBuilder, cex = 0.7, pos = 4, col = "red")

## We can see from the scatterplot of Draft versus Displacement that the yachts
## from the German Yachtbuilders seem to have both shallower draft and less displacement
## compared to the yachts from the French Yachtbuilders.

## Let us get some summary statistics using "sapply()".
## The "sapply()" function can be used to apply some other function to each column in a data frame,
## eg. sapply(mydataframe,sd) will calculate the standard deviation of each column in a dataframe "mydataframe".
sapply(yachts[2:5], mean)
sapply(yachts[2:5], sd)
## It is often interesting to calculate the means and standard deviations for just the samples from a particular group,
## for example, for the yacht samples from each YachtBuilder.
## The YachtBuilder is stored in the 1st column of the dataset "yachts".
BavariaYachts <- yachts[yachts$YachtBuilder == "Bavaria", ]
BeneteauYachts <- yachts[yachts$YachtBuilder == "Beneteau", ]
HanseYachts <- yachts[yachts$YachtBuilder == "Hanse", ]
JeanneauYachts <- yachts[yachts$YachtBuilder == "Jeanneau", ]
sapply(BavariaYachts[2:5], mean)
sapply(BeneteauYachts[2:5], mean)
sapply(HanseYachts[2:5], mean)
sapply(JeanneauYachts[2:5], mean)

## PRINCIPAL COMPONENT ANALYSIS in R
## If you want to compare different variables that have different units, are very different variances,
## it is a good idea to first standardize the variables
## You can standardize variables in R using the "scale()" function.
standardisedconcentrations <- as.data.frame(scale(yachts[2:5]))
#sapply(standardisedconcentrations[2:14])

## The purpose of principal component analysis is to find the best low-dimensional representation
## of the variation in a multivariate data set.
## For example, in the case of the yachts dataset, we have 4 measurements
## describing yachts from 4 different yacht builders.
## We can carry out a principal component analysis to investigate whether we can capture
## most of the variation between yachts using a smaller number of new variables (principal components),
## where each of these new variables is a linear combination of all or some of the 4 measurements.
help(prcomp)
yachts.pca <- prcomp(standardisedconcentrations)

## You can get a summary of the principal component analysis results
## using the "summary()" function on the output of "prcomp()":
summary(yachts.pca)

## This gives us the standard deviation of each component, and the proportion of variance explained by each component. 
## In order to decide how many principal components should be retained,
## it is common to summarise the results of a principal components analysis by making a scree plot,
## which we can do in R using the "screeplot()" function:
screeplot(yachts.pca, type="lines")

## The most obvious change in slope in the scree plot occurs at component 3, which is the "elbow" of the scree plot.
## Therefore, it could be argued based on the basis of the scree plot that the first 3 components
## should be retained.
## Another way of deciding how many components to retain is to use Kaiser's criterion:
## that we should only retain principal components for which the variance is above 1
## (when principal component analysis was applied to standardized data).
## We can check this by finding the variance of each of the principal components:
(yachts.pca$sdev)^2

## Which components are above 1?
## The values of the principal components are stored in a named element "x" of the variable returned by "prcomp()".
## This contains a matrix with the principal components,
## where the 1st column in the matrix contains the first principal component,
## the second column the second component, and so on.
## Thus, in our example, "yachts.pca$x[,1]" contains the 1st principal component,
## and "yachts.pca$x[,2]" contains the 2nd principal component.
## We can make a scatterplot of the 1st 2 principal components,
## and label the data points with the YachtBuilder that the yachts come from:
plot(yachts.pca$x[,1],yachts.pca$x[,2])
text(yachts.pca$x[,1],yachts.pca$x[,2], yachts$YachtBuilder, cex = 0.7, pos = 4, col = "red")

## re-label for clarity on plot
yachts2 <- yachts
str(yachts2)
yachts2$YachtBuilder = as.character(yachts2$YachtBuilder)
yachts2$YachtBuilder[yachts2$YachtBuilder == "Bavaria"] <- "Ba"
yachts2$YachtBuilder[yachts2$YachtBuilder == "Beneteau"] <- "Be"
yachts2$YachtBuilder[yachts2$YachtBuilder == "Hanse"] <- "H"
yachts2$YachtBuilder[yachts2$YachtBuilder == "Jeanneau"] <- "J"
View(yachts2)

## re-plot
plot(yachts.pca$x[,1],yachts.pca$x[,2])
text(yachts.pca$x[,1],yachts.pca$x[,2], yachts2$YachtBuilder, cex = 0.7, pos = 4, col = "red")


## What does this plot tell us?
## The scatterplot shows the 1st principal component on the x-axis, and the 2nd principal component on the y-axis.
## We can see from the scatterplot that German yachts have much lower values
## of the 1st principal component than French Yachts.
## Therefore, the 1st principal component separates the French yachts from the German yachts.
## The 2nd principal component demonstrates the "tightness" of the French yachts compared to the German yachts.
## Therefore, the 1st 2 principal components are reasonably useful for distinguishing yachts
## of the 4 different yacht builders.

## an alternative way to visualise the results of a PCA is as follows: 
biplot(yachts.pca)

## Interpretation: the relative location of the points can be interpreted.
## Points that are close together correspond to observations that have similar scores
## on the components displayed in the plot.
## To the extent that these components fit the data well, the points also correspond
## to observations that have similar values on the variables.
## The arrow is a vector and it points in the direction which is most like the
## variable represented by the vector.
## This is the direction which has the highest squared multiple correlation
## with the principal components.
## The length of the vector is proportional to the squared multiple correlation
## between the fitted values for the variable and the variable itself.
