# Quiz2
mail@darrenbishop.com  
12 March 2016  


```r
library(knitr)
library(ggplot2)
library(gridExtra)
library(Hmisc)

opts_chunk$set(message=FALSE, warning = FALSE, fig.path="figures/", fig.width=10, fig.align = "center")
```

## Question 1

> Which of the following commands will create non-overlapping training and test sets with about 50% of the observations assigned to each?


```r
library(caret)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
```

***Answer:***

> adData = data.frame(diagnosis,predictors)
> trainIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
> training = adData[trainIndex,]
> testing = adData[-trainIndex,]

# Question 2

> Make a plot of the outcome (CompressiveStrength) versus the index of the samples. Color by each of the variables in the data set (you may find the cut2() function in the Hmisc package useful for turning continuous covariates into factors). What do you notice in these plots?


```r
library(caret)
library(AppliedPredictiveModeling)
data(concrete)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]


modFit = train(CompressiveStrength ~ ., method = "lm", data = training)
finalModel = modFit$finalModel
print(modFit)
```

```
## Linear Regression 
## 
## 774 samples
##   8 predictor
## 
## No pre-processing
## Resampling: Bootstrapped (25 reps) 
## Summary of sample sizes: 774, 774, 774, 774, 774, 774, ... 
## Resampling results
## 
##   RMSE      Rsquared   RMSE SD    Rsquared SD
##   10.54379  0.5949013  0.3394799  0.02624981 
## 
## 
```

```r
colnames(training)
```

```
## [1] "Cement"              "BlastFurnaceSlag"    "FlyAsh"             
## [4] "Water"               "Superplasticizer"    "CoarseAggregate"    
## [7] "FineAggregate"       "Age"                 "CompressiveStrength"
```

```r
p = ggplot(data = training, aes(x = 1:nrow(training), y = CompressiveStrength)) +
    labs(x = "Index") +
    theme(legend.position="top")

# grid.arrange(ncol = 1,
  p + geom_point(aes(color = cut2(Age)))
```

<img src="figures/unnamed-chunk-2-1.png" title="" alt="" style="display: block; margin: auto;" />

```r
  p + geom_point(aes(color = cut2(FlyAsh)))
```

<img src="figures/unnamed-chunk-2-2.png" title="" alt="" style="display: block; margin: auto;" />

```r
  p + geom_point(aes(color = cut2(Water)))
```

<img src="figures/unnamed-chunk-2-3.png" title="" alt="" style="display: block; margin: auto;" />

```r
  p + geom_point(aes(color = cut2(FineAggregate)))
```

<img src="figures/unnamed-chunk-2-4.png" title="" alt="" style="display: block; margin: auto;" />

```r
  p + geom_point(aes(color = cut2(Cement)))
```

<img src="figures/unnamed-chunk-2-5.png" title="" alt="" style="display: block; margin: auto;" />

```r
  p + geom_point(aes(color = cut2(Superplasticizer)))
```

<img src="figures/unnamed-chunk-2-6.png" title="" alt="" style="display: block; margin: auto;" />

```r
  p + geom_point(aes(color = cut2(CoarseAggregate)))
```

<img src="figures/unnamed-chunk-2-7.png" title="" alt="" style="display: block; margin: auto;" />

```r
  p + geom_point(aes(color = cut2(BlastFurnaceSlag)))
```

<img src="figures/unnamed-chunk-2-8.png" title="" alt="" style="display: block; margin: auto;" />

```r
# )
```

***Answer:***

> There is a non-random pattern in the plot of the outcome versus index that does not appear to be perfectly explained by any predictor suggesting a variable may be missing.

# Question 3

> Make a histogram and confirm the SuperPlasticizer variable is skewed. Normally you might use the log transform to try to make the data more symmetric. Why would that be a poor choice for this variable?


```r
library(caret)
library(AppliedPredictiveModeling)
data(concrete)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]


grid.arrange(nrow = 2,
ggplot(data = training, aes(x = Superplasticizer)) +
  geom_histogram(),
ggplot(data = training, aes(x = log(Superplasticizer + 1))) +
  geom_histogram()
)
```

<img src="figures/unnamed-chunk-3-1.png" title="" alt="" style="display: block; margin: auto;" />
***Answer:*** 

> There are a large number of values that are the same and even if you took the log(SuperPlasticizer + 1) they would still all be identical so the distribution would not be symmetric.

# Question 4

> Find all the predictor variables in the training set that begin with IL. Perform principal components on these variables with the preProcess() function from the caret package. Calculate the number of principal components needed to capture 80% of the variance. How many are there?


```r
library(caret)
library(AppliedPredictiveModeling)
library(dplyr)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis, predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]


training.IL = training %>%
    dplyr::select(starts_with("IL"))

preProcess(training.IL, method = "pca", thresh = 0.9)
```

```
## Created from 251 samples and 12 variables
## 
## Pre-processing:
##   - centered (12)
##   - ignored (0)
##   - principal component signal extraction (12)
##   - scaled (12)
## 
## PCA needed 9 components to capture 90 percent of the variance
```

***Answer:***

> 7 for 80%
> 
> 9 for 90%

# Question 5

> Create a training data set consisting of only the predictors with variable names beginning with IL and the diagnosis. Build two predictive models, one using the predictors as they are and one using PCA with principal components explaining 80% of the variance in the predictors. Use method="glm" in the train function.


```r
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]


training.ILD = training %>%
    dplyr::select(diagnosis, starts_with("IL"))

train(diagnosis ~ ., method = "glm", data = training.ILD)
```

```
## Generalized Linear Model 
## 
## 251 samples
##  12 predictor
##   2 classes: 'Impaired', 'Control' 
## 
## No pre-processing
## Resampling: Bootstrapped (25 reps) 
## Summary of sample sizes: 251, 251, 251, 251, 251, 251, ... 
## Resampling results
## 
##   Accuracy   Kappa       Accuracy SD  Kappa SD  
##   0.6861681  0.05592848  0.04317554   0.08835269
## 
## 
```

```r
preProc = preProcess(training.ILD, method = "pca", thresh = 0.8)
training.pca = predict(preProc, training.ILD)
train(diagnosis ~ ., method = "glm", data = training.pca)
```

```
## Generalized Linear Model 
## 
## 251 samples
##   7 predictor
##   2 classes: 'Impaired', 'Control' 
## 
## No pre-processing
## Resampling: Bootstrapped (25 reps) 
## Summary of sample sizes: 251, 251, 251, 251, 251, 251, ... 
## Resampling results
## 
##   Accuracy   Kappa       Accuracy SD  Kappa SD  
##   0.6918527  0.03802848  0.03684473   0.08823526
## 
## 
```

```r
train(diagnosis ~ ., data = training.ILD,
      method = "glm", preProcess = "pca",
      trControl = trainControl(preProcOptions = list(thresh = 0.8)))
```

```
## Generalized Linear Model 
## 
## 251 samples
##  12 predictor
##   2 classes: 'Impaired', 'Control' 
## 
## Pre-processing: principal component signal extraction (12), centered
##  (12), scaled (12) 
## Resampling: Bootstrapped (25 reps) 
## Summary of sample sizes: 251, 251, 251, 251, 251, 251, ... 
## Resampling results
## 
##   Accuracy   Kappa       Accuracy SD  Kappa SD  
##   0.7169443  0.07755798  0.04000275   0.07015442
## 
## 
```

***Answer:***

> Non-PCA Accuracy: 0.65
> 
> PCA Accuracy: 0.72
