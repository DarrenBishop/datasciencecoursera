# Quiz4
mail@darrenbishop.com  
26 March 2016  


```r
library(knitr)
library(ggplot2)
library(ggdendro)
library(gridExtra)
library(rpart.plot)
library(Hmisc)
library(caret)

opts_chunk$set(message=FALSE, warning = FALSE, fig.path="figures/", fig.width=10, fig.align = "center")
```

## Question 1

> Load the vowel.train and vowel.test data sets:


```r
library(ElemStatLearn)

data(vowel.train)
data(vowel.test)
```

> Set the variable y to be a factor variable in both the training and test set.
> Then set the seed to 33833.
> Fit (1) a random forest predictor relating the factor variable y to the remaining variables and (2) a boosted predictor using the "gbm" method.
> Fit these both with the train() command in the caret package.


```r
vowel.train$y = as.factor(vowel.train$y)
vowel.test$y = as.factor(vowel.test$y)

vowel.data = rbind(vowel.train, vowel.test)

set.seed(33833)
# rfModel = randomForest(y ~ ., data = vowel.train, proximity = TRUE)
rfModel = train(y ~ ., data = vowel.train, method = "rf", proximity = TRUE)
gbmModel = train(y ~ ., data = vowel.train, method = "gbm", verbose = FALSE)
```

> What are the accuracies for the two approaches on the test data set?
> What is the accuracy among the test set samples where the two methods agree?


```r
rfPred = predict(rfModel, newdata = vowel.test)
gbmPred = predict(gbmModel, newdata = vowel.test)

confusionMatrix(rfPred, vowel.test$y)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction  1  2  3  4  5  6  7  8  9 10 11
##         1  34  3  0  0  0  0  0  0  0  1  0
##         2   7 21  3  0  0  0  0  0  1 13  1
##         3   1 14 32  3  0  1  0  0  0  5  2
##         4   0  0  2 29  3  0  0  0  0  0  2
##         5   0  0  0  0 20  8  8  0  0  0  0
##         6   0  0  4  9 15 22  3  0  0  0  5
##         7   0  0  0  0  3  0 28  6  5  0  3
##         8   0  0  0  0  0  0  0 32  6  0  0
##         9   0  4  0  0  0  0  2  4 23  1 11
##         10  0  0  0  0  0  0  1  0  2 22  0
##         11  0  0  1  1  1 11  0  0  5  0 18
## 
## Overall Statistics
##                                          
##                Accuracy : 0.6082         
##                  95% CI : (0.5621, 0.653)
##     No Information Rate : 0.0909         
##     P-Value [Acc > NIR] : < 2.2e-16      
##                                          
##                   Kappa : 0.569          
##  Mcnemar's Test P-Value : NA             
## 
## Statistics by Class:
## 
##                      Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6
## Sensitivity           0.80952  0.50000  0.76190  0.69048  0.47619  0.52381
## Specificity           0.99048  0.94048  0.93810  0.98333  0.96190  0.91429
## Pos Pred Value        0.89474  0.45652  0.55172  0.80556  0.55556  0.37931
## Neg Pred Value        0.98113  0.94952  0.97525  0.96948  0.94836  0.95050
## Prevalence            0.09091  0.09091  0.09091  0.09091  0.09091  0.09091
## Detection Rate        0.07359  0.04545  0.06926  0.06277  0.04329  0.04762
## Detection Prevalence  0.08225  0.09957  0.12554  0.07792  0.07792  0.12554
## Balanced Accuracy     0.90000  0.72024  0.85000  0.83690  0.71905  0.71905
##                      Class: 7 Class: 8 Class: 9 Class: 10 Class: 11
## Sensitivity           0.66667  0.76190  0.54762   0.52381   0.42857
## Specificity           0.95952  0.98571  0.94762   0.99286   0.95476
## Pos Pred Value        0.62222  0.84211  0.51111   0.88000   0.48649
## Neg Pred Value        0.96643  0.97642  0.95444   0.95423   0.94353
## Prevalence            0.09091  0.09091  0.09091   0.09091   0.09091
## Detection Rate        0.06061  0.06926  0.04978   0.04762   0.03896
## Detection Prevalence  0.09740  0.08225  0.09740   0.05411   0.08009
## Balanced Accuracy     0.81310  0.87381  0.74762   0.75833   0.69167
```

```r
confusionMatrix(gbmPred, vowel.test$y)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction  1  2  3  4  5  6  7  8  9 10 11
##         1  29  1  0  0  0  0  0  0  0  3  0
##         2  10 19  1  0  0  0  1  0  0 16  0
##         3   2 13 12  3  0  0  0  0  0  0  0
##         4   0  2 10 19  3  0  1  0  0  0  1
##         5   0  0  0  1 16  6  1  0  0  0  0
##         6   0  1 15 18 11 27  2  0  3  0 12
##         7   0  0  1  0  8  2 34  8  4  0 12
##         8   0  0  0  0  0  0  2 28  8  0  0
##         9   0  4  0  0  0  0  0  6 27  2 13
##         10  1  0  0  0  0  0  0  0  0 21  0
##         11  0  2  3  1  4  7  1  0  0  0  4
## 
## Overall Statistics
##                                           
##                Accuracy : 0.5108          
##                  95% CI : (0.4642, 0.5573)
##     No Information Rate : 0.0909          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.4619          
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6
## Sensitivity           0.69048  0.45238  0.28571  0.45238  0.38095  0.64286
## Specificity           0.99048  0.93333  0.95714  0.95952  0.98095  0.85238
## Pos Pred Value        0.87879  0.40426  0.40000  0.52778  0.66667  0.30337
## Neg Pred Value        0.96970  0.94458  0.93056  0.94601  0.94064  0.95979
## Prevalence            0.09091  0.09091  0.09091  0.09091  0.09091  0.09091
## Detection Rate        0.06277  0.04113  0.02597  0.04113  0.03463  0.05844
## Detection Prevalence  0.07143  0.10173  0.06494  0.07792  0.05195  0.19264
## Balanced Accuracy     0.84048  0.69286  0.62143  0.70595  0.68095  0.74762
##                      Class: 7 Class: 8 Class: 9 Class: 10 Class: 11
## Sensitivity           0.80952  0.66667  0.64286   0.50000  0.095238
## Specificity           0.91667  0.97619  0.94048   0.99762  0.957143
## Pos Pred Value        0.49275  0.73684  0.51923   0.95455  0.181818
## Neg Pred Value        0.97964  0.96698  0.96341   0.95227  0.913636
## Prevalence            0.09091  0.09091  0.09091   0.09091  0.090909
## Detection Rate        0.07359  0.06061  0.05844   0.04545  0.008658
## Detection Prevalence  0.14935  0.08225  0.11255   0.04762  0.047619
## Balanced Accuracy     0.86310  0.82143  0.79167   0.74881  0.526190
```

```r
confusionMatrix(rfPred, gbmPred)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction  1  2  3  4  5  6  7  8  9 10 11
##         1  27  6  2  0  0  0  0  0  0  2  1
##         2   3 37  3  0  0  1  0  0  0  0  2
##         3   3  0 25 10  0 13  3  0  1  2  1
##         4   0  0  0 23  0 12  0  0  0  0  1
##         5   0  0  0  0 18  3 14  1  0  0  0
##         6   0  0  0  0  5 46  4  0  0  0  3
##         7   0  1  0  0  1  0 40  1  1  0  1
##         8   0  0  0  0  0  0  2 34  2  0  0
##         9   0  0  0  2  0  3  1  2 37  0  0
##         10  0  3  0  0  0  0  1  0  3 18  0
##         11  0  0  0  1  0 11  4  0  8  0 13
## 
## Overall Statistics
##                                           
##                Accuracy : 0.6883          
##                  95% CI : (0.6439, 0.7303)
##     No Information Rate : 0.1926          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.6548          
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6
## Sensitivity           0.81818  0.78723  0.83333  0.63889  0.75000  0.51685
## Specificity           0.97436  0.97831  0.92361  0.96948  0.95890  0.96783
## Pos Pred Value        0.71053  0.80435  0.43103  0.63889  0.50000  0.79310
## Neg Pred Value        0.98585  0.97596  0.98762  0.96948  0.98592  0.89356
## Prevalence            0.07143  0.10173  0.06494  0.07792  0.05195  0.19264
## Detection Rate        0.05844  0.08009  0.05411  0.04978  0.03896  0.09957
## Detection Prevalence  0.08225  0.09957  0.12554  0.07792  0.07792  0.12554
## Balanced Accuracy     0.89627  0.88277  0.87847  0.80419  0.85445  0.74234
##                      Class: 7 Class: 8 Class: 9 Class: 10 Class: 11
## Sensitivity           0.57971  0.89474  0.71154   0.81818   0.59091
## Specificity           0.98728  0.99057  0.98049   0.98409   0.94545
## Pos Pred Value        0.88889  0.89474  0.82222   0.72000   0.35135
## Neg Pred Value        0.93046  0.99057  0.96403   0.99085   0.97882
## Prevalence            0.14935  0.08225  0.11255   0.04762   0.04762
## Detection Rate        0.08658  0.07359  0.08009   0.03896   0.02814
## Detection Prevalence  0.09740  0.08225  0.09740   0.05411   0.08009
## Balanced Accuracy     0.78349  0.94265  0.84601   0.90114   0.76818
```

***Answer:***

> RF Accuracy = 0.6082
> 
> GBM Accuracy = 0.5152
> 
> Agreement Accuracy = 0.6361

### Question 2

> Load the Alzheimer's data using the following commands


```r
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)

inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
```

> Set the seed to 62433 and predict diagnosis with all the other variables using a random forest ("rf"), boosted trees ("gbm") and linear discriminant analysis ("lda") model.
> Stack the predictions together using random forests ("rf").
> What is the resulting accuracy on the test set? Is it better or worse than each of the individual predictions?


```r
set.seed(62433)
# rfModel = train(diagnosis ~ ., data = training, method = "rf", number = 3, proximity = TRUE)
rf2Model = train(diagnosis ~ ., data = training, method = "rf", number = 3, trControl = trainControl(method = "cv"), proximity = TRUE)
gbmModel = train(diagnosis ~ ., data = training, method = "gbm", verbose = FALSE)
ldaModel = train(diagnosis ~ ., data = training, method = "lda", verbose = FALSE)

m.vote <- function(n, p) {
    m = n %/% 2 + 1
    terms = sapply(m:n, function(k) choose(n, k) * p^(k) * (1-p)^(n-k))
    sum(terms)
}

rfPred = predict(rf2Model, testing)
gbmPred = predict(gbmModel, testing)
ldaPred = predict(ldaModel, testing)

stackData = data.frame(rfPred, gbmPred, ldaPred, diagnosis = testing$diagnosis)

stackModel = train(diagnosis ~ ., data = stackData, method = "rf", number = 3, proximity = TRUE)
```

```
## note: only 2 unique complexity parameters in default grid. Truncating the grid to 2 .
```

```r
stackPred = predict(stackModel, testing)

confusionMatrix(rfPred, testing$diagnosis)$overall['Accuracy']
```

```
##  Accuracy 
## 0.7926829
```

```r
confusionMatrix(gbmPred, testing$diagnosis)$overall['Accuracy']
```

```
##  Accuracy 
## 0.7804878
```

```r
confusionMatrix(ldaPred, testing$diagnosis)$overall['Accuracy']
```

```
##  Accuracy 
## 0.7682927
```

```r
confusionMatrix(stackPred, testing$diagnosis)$overall['Accuracy']
```

```
## Accuracy 
## 0.804878
```

***Answer:***

> Stacked Accuracy: 0.80 is better than random forests and lda and the same as boosting.

### Question 3

> Load the concrete data with the commands:


```r
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
```

> Set the seed to 233 and fit a lasso model to predict Compressive Strength.
> Which variable is the last coefficient to be set to zero as the penalty increases?
> (Hint: it may be useful to look up ?plot.enet).


```r
set.seed(233)
model = train(CompressiveStrength ~ ., data = concrete, method = "lasso")
summary(model$finalModel)
```

```
##             Length Class      Mode     
## call         4     -none-     call     
## actions     11     -none-     list     
## allset       8     -none-     numeric  
## beta.pure   88     -none-     numeric  
## vn           8     -none-     character
## mu           1     -none-     numeric  
## normx        8     -none-     numeric  
## meanx        8     -none-     numeric  
## lambda       1     -none-     numeric  
## L1norm      11     -none-     numeric  
## penalty     11     -none-     numeric  
## df          11     -none-     numeric  
## Cp          11     -none-     numeric  
## sigma2       1     -none-     numeric  
## xNames       8     -none-     character
## problemType  1     -none-     character
## tuneValue    1     data.frame list     
## obsLevels    1     -none-     logical
```

```r
plot(model$finalModel)
```

<img src="figures/unnamed-chunk-7-1.png" title="" alt="" style="display: block; margin: auto;" />

***Answer:***

> Cement

### Question 4

> Load the data on the number of visitors to the instructors blog from here:


```r
library(forecast)
library(lubridate) # For year() function below

if (!file.exists("gaData.csv")) {
    download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv", destfile = "gaData.csv")
}
dat = read.csv("gaData.csv")

training = dat[year(dat$date) < 2012,]
testing = dat[year(dat$date) > 2011,]
tstrain = ts(training$visitsTumblr)
```

> Fit a model using the bats() function in the forecast package to the training time series.
> Then forecast this model for the remaining time points.
> For how many of the testing points is the true value within the 95% prediction interval bounds?


```r
tsmodel = bats(tstrain)
f = forecast(tsmodel, nrow(testing))
plot(f)
lines(ts(testing$visitsTumblr, start = 366), col= "red")
```

<img src="figures/unnamed-chunk-9-1.png" title="" alt="" style="display: block; margin: auto;" />

```r
accuracy(f, ts(testing$visitsTumblr, start = 366))
```

```
##                     ME     RMSE       MAE      MPE     MAPE      MASE
## Training set  10.39787 258.7813  40.22707     -Inf      Inf 0.8437138
## Test set     185.20149 301.2620 205.87807 26.06409 40.76475 4.3180420
##                   ACF1 Theil's U
## Training set 0.0121040        NA
## Test set     0.4751213  1.092536
```


***Answer:***

> 96%

### Question 5

> Load the concrete data with the commands:


```r
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
```

> Set the seed to 325 and fit a support vector machine using the e1071 package to predict Compressive Strength using the default settings.
> Predict on the testing set.
> What is the RMSE?


```r
library(e1071)
set.seed(325)
model = svm(CompressiveStrength ~ ., data = training)
pred = predict(model, newdata = testing)
accuracy(pred, testing$CompressiveStrength)
```

```
##                 ME     RMSE      MAE       MPE     MAPE
## Test set 0.1682863 6.715009 5.120835 -7.102348 19.27739
```

*** Answer:***

> 6.715009


```r
library(quantmod)
from.dat = as.Date("01/01/08", format = "%m/%d/%y")
to.dat = as.Date("12/31/13", format = "%m/%d/%y")
getSymbols("GOOG", src = "google", from = from.dat, to = to.dat)
```

```
## [1] "GOOG"
```

```r
mGoog = to.monthly(OHLC(GOOG))
googOpen = Op(mGoog)
tsGoog = ts(googOpen, frequency = 12)
plot(tsGoog, xlab = "Years+1", ylab = "GOOG")
```

<img src="figures/unnamed-chunk-12-1.png" title="" alt="" style="display: block; margin: auto;" />

```r
plot(decompose(tsGoog), xlab="Years+1")
```

<img src="figures/unnamed-chunk-12-2.png" title="" alt="" style="display: block; margin: auto;" />

