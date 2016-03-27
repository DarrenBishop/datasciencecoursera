# Quiz3
mail@darrenbishop.com  
16 March 2016  


```r
library(knitr)
library(ggplot2)
library(ggdendro)
library(gridExtra)
library(rpart.plot)
library(Hmisc)

opts_chunk$set(message=FALSE, warning = FALSE, fig.path="figures/", fig.width=10, fig.align = "center")
```

## Question 1

> 1. Subset the data to a training set and testing set based on the Case variable in the data set.
> 
> 2. Set the seed to 125 and fit a CART model with the rpart method using all predictor variables and default caret settings.
> 
> 3. In the final model what would be the final model prediction for cases with the following variable values:
> 
> a. TotalIntench2 = 23,000; FiberWidthCh1 = 10; PerimStatusCh1=2
> 
> b. TotalIntench2 = 50,000; FiberWidthCh1 = 10;VarIntenCh4 = 100
> 
> c. TotalIntench2 = 57,000; FiberWidthCh1 = 8;VarIntenCh4 = 100
> 
> d. FiberWidthCh1 = 8;VarIntenCh4 = 100; PerimStatusCh1=2



```r
library(caret)
library(AppliedPredictiveModeling)
data(segmentationOriginal)

trainIndex = createDataPartition(segmentationOriginal$Case, p = 0.7, list = FALSE)
training = segmentationOriginal[trainIndex,]
testing = segmentationOriginal[-trainIndex,]

set.seed(125)
modFit = train(Class ~ ., method = "rpart", data = training)
# tdata = rpart(Class ~ ., method = "class", data = training)
# prp(tdata)
finalModel = modFit$finalModel
print(finalModel)
```

```
## n= 1414 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
## 1) root 1414 506 PS (0.64214993 0.35785007)  
##   2) TotalIntenCh2< 47244 674  55 PS (0.91839763 0.08160237) *
##   3) TotalIntenCh2>=47244 740 289 WS (0.39054054 0.60945946)  
##     6) FiberWidthCh1< 11.19756 310 126 PS (0.59354839 0.40645161) *
##     7) FiberWidthCh1>=11.19756 430 105 WS (0.24418605 0.75581395) *
```

```r
prp(finalModel)
```

<img src="figures/unnamed-chunk-1-1.png" title="" alt="" style="display: block; margin: auto;" />

```r
# ddata = dendro_data(finalModel)
# ggplot() + 
#     geom_segment(data = ddata$segments, aes(x = x, y = y, xend = xend, yend = yend)) + 
#     geom_text(data = ddata$labels, aes(x = x, y = y, label = label), size = 5, vjust = -0.4) +
#     geom_text(data = ddata$leaf_labels, aes(x = x, y = y, label = label), size = 5, vjust = 1) +
#     theme_dendro()
```

***Answer:***

> a. PS
> 
> b. WS
> 
> c. PS
> 
> d. Not possible to predict

## Question 2

> If K is small in a K-fold cross validation is the bias in the estimate of out-of-sample (test set) accuracy smaller or bigger?
> If K is small is the variance in the estimate of out-of-sample (test set) accuracy smaller or bigger.
> Is K large or small in leave one out cross validation?

***Answer:***

> The bias is larger and the variance is smaller. Under leave one out cross validation K is equal to the sample size.

## Question 3


```r
library(caret)
library(pgmm)
data(olive)
olive = olive[,-1]
```

> These data contain information on 572 different Italian olive oils from multiple regions in Italy.
> Fit a classification tree where Area is the outcome variable.
> Then predict the value of area for the following data frame using the tree command with all defaults
> What is the resulting prediction? Is the resulting prediction strange? Why or why not?


```r
modFit = train(Area ~ ., data = olive, method = "rf", prox = TRUE)
modFit
```

```
## Random Forest 
## 
## 572 samples
##   8 predictor
## 
## No pre-processing
## Resampling: Bootstrapped (25 reps) 
## 
## Summary of sample sizes: 572, 572, 572, 572, 572, 572, ... 
## 
## Resampling results across tuning parameters:
## 
##   mtry  RMSE       Rsquared   RMSE SD     Rsquared SD
##   2     0.4777018  0.9608779  0.05145482  0.009427885
##   5     0.3996253  0.9713231  0.03727615  0.006048262
##   8     0.4182004  0.9681267  0.05729694  0.009580648
## 
## RMSE was used to select the optimal model using  the smallest value.
## The final value used for the model was mtry = 5.
```

```r
finalModel = modFit$finalModel

predict(finalModel, newdata = as.data.frame(t(colMeans(olive))))
```

```
##        1 
## 2.643567
```

***Answer:***

> 2.783. It is strange because Area should be a qualitative variable - but tree is reporting the average value of Area as a numeric variable in the leaf predicted for newdata

## Question 4

> Load the South Africa Heart Disease Data and create training and test sets with the following code:


```r
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]
```

> Then set the seed to 13234 and fit a logistic regression model (method="glm", be sure to specify family="binomial") with Coronary Heart Disease (chd) as the outcome and age at onset, current alcohol consumption, obesity levels, cumulative tabacco, type-A behavior, and low density lipoprotein cholesterol as predictors. Calculate the misclassification rate for your model using this function and a prediction on the "response" scale:


```r
missClass = function(values,prediction) {
    sum(((prediction > 0.5) * 1) != values) / length(values)
}
```

> What is the misclassification rate on the training set? What is the misclassification rate on the test set?


```r
set.seed(13234)
modFit = train(chd ~ age + alcohol + obesity + tobacco + typea + ldl, data = trainSA, method = "glm", family = "binomial")
modFit
```

```
## Generalized Linear Model 
## 
## 231 samples
##   9 predictor
## 
## No pre-processing
## Resampling: Bootstrapped (25 reps) 
## 
## Summary of sample sizes: 231, 231, 231, 231, 231, 231, ... 
## 
## Resampling results
## 
##   RMSE       Rsquared   RMSE SD    Rsquared SD
##   0.4366982  0.2000453  0.0239573  0.06521095 
## 
## 
```

```r
finalModel = modFit$finalModel

missClass(trainSA$chd, predict(finalModel, newdata = trainSA, type = "response"))
```

```
## [1] 0.2727273
```

```r
missClass(trainSA$chd, predict(finalModel, newdata = testSA, type = "response"))
```

```
## [1] 0.4761905
```

***Answer:***

> Training miss-classification: 0.2727273
> Test miss-classification: 0.4761905

## Question 5

> Load the vowel.train and vowel.test data sets:


```r
library(dplyr)
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
```

> Set the variable y to be a factor variable in both the training and test set. Then set the seed to 33833.
> Fit a random forest predictor relating the factor variable y to the remaining variables.
> Read about variable importance in random forests here: http://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm#ooberr
> The caret package uses by default the Gini importance.
> 
> Calculate the variable importance using the varImp function in the caret package.
> What is the order of variable importance?


```r
vowel.train$y = as.factor(vowel.train$y)
vowel.test$y = as.factor(vowel.test$y)

set.seed(33833)
modFit = train(y ~ ., data = vowel.train, method = "rf")
varImp(modFit)
```

```
## rf variable importance
## 
##      Overall
## x.2  100.000
## x.1   98.834
## x.5   38.651
## x.6   25.628
## x.8   19.065
## x.4    8.397
## x.9    5.417
## x.3    4.728
## x.7    1.252
## x.10   0.000
```

```r
set.seed(33833)
finalModel = randomForest(y ~ ., data = vowel.train, proximity = TRUE)
t(varImp(finalModel) %>% add_rownames() %>% arrange(desc(Overall)))
```

```
##         [,1]       [,2]       [,3]       [,4]       [,5]       [,6]      
## rowname "x.2"      "x.1"      "x.5"      "x.6"      "x.8"      "x.4"     
## Overall "91.24009" "89.12864" "50.25539" "43.33148" "42.92470" "34.24433"
##         [,7]       [,8]       [,9]       [,10]     
## rowname "x.9"      "x.3"      "x.7"      "x.10"    
## Overall "33.37031" "33.08111" "31.88132" "29.59956"
```

***Answer:***

> The order of the variables is:
> 
> x.2, x.1, x.5, x.6, x.8, x.4, x.9, x.3, x.7, x.10
