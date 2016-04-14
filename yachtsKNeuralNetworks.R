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

## scatterplot
install.packages("ggvis")
library(ggvis)
yachts %>% ggvis(~LOA, ~Beam, fill = ~YachtBuilder) %>% layer_points()
## Note: there appears to be quite a strong positive correlation between LOA and Beam
## for the Bavaria yachts and the Hanse yachts,
## There appears to be a moderate positive correlation between LOA and Beam
## for the Beneteau and Jeanneau yachts

## Now generate a scatterplot that maps the draft and the displacement:
yachts %>% ggvis(~Draft, ~Displacement, fill = ~YachtBuilder) %>% layer_points()
## Note: the scatterplot indicates a strong positive correlation between the draft and the displacement
## for all four (4) yacht builders.

## This yachts dataset can be used for classification (an example of predictive modeling).
## The first attribute of the dataset (i.e. the column labelled "YachtBuilder") will be the target variable
## (i.e. YachtBuilder is the variable that we want to predict in this instance).

## install the class package class
install.packages("class")
library(class)

## Normalise the dataset
summary(yachts)
## The yachts data set needs to be normalised:
## the LOA attribute has values that go from 9.02 to 22.24
## and Beam contains values from 2.45 to 6.20,
## while Draft values range from 0.93 to 8.57,
## yet Displacement ranges from 7088 to 160650.
## So, Displacement's influence will tend to overpower the influences of the other three (3) attributes.
## Thus, there is a need to normalise the dataset, i.e. adjust the ranges of all attributes,
## so that distances between attributes with larger ranges will not be over-emphasised.

## create normalise function:
normalise <- function(x)
  {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
  } ## end function

## place results of normalisation in a data frame using as.data.frame()
## the function lapply() returns a list of the same length as the dataset,
## each element of that list is the result of the application of the normalise argument to the dataset
## For the yachts dataset, the normalise argument is applied to the four (4) numerical measurements
## of the yachts dataset (LOA, Beam, Draft, Displacement),
## the results are placed into a data frame:
yachtsNormalised <- as.data.frame(lapply(yachts[2:5], normalise))

## check normalised dataset
View(yachtsNormalised)
summary(yachtsNormalised)
## Now, values of all attributes are contained within the range of 0.0 to 1.0

set.seed(2345)

ind <- sample(2, nrow(yachtsNormalised), replace = TRUE, prob = c(0.75, 0.25))

## create test dataset & training dataset
## use 3/4 in training dataset & 1/4 in test dataset
yachtsTraining <- yachtsNormalised[ind == 1, 1:4]
yachtsTest <- yachtsNormalised[ind == 2, 1:4]

## check
View(yachtsTraining)
View(yachtsTest)

## Note: do NOT need to take into account ALL attributes to form the training set and test set.
## Only needed to consider LOA, Beam, Draft & Displacement.
## ... because want to predict the 1st attribute, YachtBuilder (this is the target variable).
## However, the YachtBuilder attribute must be incorporated into the KNN algorithm,
## ... otherwise there will never be any prediction for it.
## Therefore, need to store the class labels in factor vectors and divide them across the training and test sets.
## Create a blank 5th column
yachtsTrainLabels <- yachts[ind == 1, 1]
yachtsTestLabels <- yachts[ind == 2, 1]
View(yachtsTrainLabels)
View(yachtsTestLabels)

## To build the classifier, take the KNN() function then add some arguments to it,
yachtsPrediction <- knn(train = yachtsTraining, test = yachtsTest, cl = yachtsTrainLabels, k = 3)

## Store into yachtsPrediction the KNN() function that takes as arguments the training set, the test set,
## the train labels and the amount of neighbours seeking to find with this algorithm.
## The result of this function is a factor vector with the predicted classes for each row of the test data.

## Note: do NOT insert the test labels:
## ... these will be used to see whether the model is good at predicting the actual classes of the instances!

## Retrieve the result of the KNN() function
## (or use write.csv to export to a csv file)
## prediction values
yachtsPrediction
View(yachtsPrediction)
## test labels
yachtsTestLabels
View(yachtsTestLabels)
## datapoints 39, 45, 55 & 65 have been mis-classifed
## i.e. 4 out of 66 are mis-classified (or approximately 6%)

## EVALUATION OF THE MODEL
## An essential next step in machine learning is the evaluation of the model's performance.
## In other words, need to analyze the degree of correctness of the model's predictions.
## For a more abstract view, simply just compare the results of yachtsPrediction to the
## yachtsTestLabels defined above:
## This will give some indication of the model's performance, however,
## the statisctal analysis should be investigated more thoroughly, as follows:

## import the package gmodels:
install.packages("gmodels")
library(gmodels)

## Now, make a cross tabulation or a contingency table.
## This type of table is often used to understand the relationship between 2 variables.
## The goal is to understand how the classes of the test data (stored in yachtsTestLabels)
## relate to the model that is stored in yachtsPrediction:
CrossTable(x = yachtsTestLabels, y = yachtsPrediction, prop.chisq = FALSE)

## Note that the last argument prop.chisq indicates whether or not the chi-square contribution
## of each cell is included.
## The chi-square statistic is the sum of the contributions from each of the individual cells
## and is used to decide whether the difference between the observed and the expected values
## is significant.

## From this table, you can derive the number of correct and incorrect predictions:
## 2 instances from the test set were labelled Bavaria by the model,
## when in actual fact these yachts were from the yacht builder Hanse, and
## 2 instances from the test set were labelled Beneteau by the model,
## when in actual fact these yachts were from the yacht builder Jeaneau.
## This can be seen by looking at the first row of the "Jeaneau" yacht-builder in the yachtsTestLabels column.
## In all other cases, correct predictions were made.
## Conclusion: the model's performance is very good and there is no need to improve the model.
