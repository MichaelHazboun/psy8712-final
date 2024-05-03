# Hypothesis 1: Are age and income correlated? 
# Hypothesis 2: Is income different based on how scientific you believe astrology to be, and if yes, which levels?
# RQ 1: a full model with everything, but other variables of income, will predict income better (have a higher R squared) than a reduced model with only 4 variables (number of pets, believes astrology is science, age and comparison to parents income at current age.)

# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
library(caret)
library(parallel) 
library(doParallel)

# Data Import and Cleaning
gss_2018_tbl<-read_sav(file="../data/GSS2018.sav") %>% #used read_sav over other options because this is a .sav file and felt the most appropriate tool to use (it's literally built for it)
  # select(-where(~mean(is.na(.))>0.75)) #used this initially to help discover my desired variables have at least 25% of the variables filled (if it wasn't in here but in the gss code book, then I don't want it). Select to pick the variables (just easy to use), then the criteria was that they didn't have 75% emptiness, and that was a quick easy and logical string to do that.
    select(NUMPETS, INCOME, PARSOL, ASTROSCI, AGE) %>% #Used select because it was easy to use, kept the three variables of interest and my desired DV.
  drop_na((INCOME))%>% #drop_na is one of the few ways we can interact with NAs and I wanted to drop them, easiest function that came to mind
  mutate(across(everything(), as.numeric))%>% #To make sure that everything works I need to make them numeric, doing mutate(across(everything())) was just simpler an easier than doing it with an apply (i'm not even sure if an apply function works, but I know this does and I don't see a reason why I should change it to test it right now)
  mutate(ASTROSCI= as.factor(ASTROSCI))

gss_2018_tbl_2<-read_sav(file="../data/GSS2018.sav") %>% 
  select(-where(~mean(is.na(.))>0.75)) %>% 
  select(-INCOM16,-INCOME16,-RINCOM16,-RINCOME) %>% #got rid of all the other incomes
  drop_na((INCOME))%>% 
  mutate(across(everything(), as.numeric)) 

#Visualization
gss_2018_tbl %>% #got a warning message that said it removed rows, those rows are rows where income is astroci is NA so it's fine
ggplot(aes(ASTROSCI,INCOME))+ #Used ggplot over base, because it's easier to make the graphs look pretty 
  geom_jitter(width=0.3, height = 0.3)+ #added .3 jitter on both height and width because the base jitter lost the definition of each income bracket and smaller values were too dense to show true scale
  geom_smooth(method="lm") # added a line to show if any trends are present, left the SE bands because they show were most of the data is concentrated.
#used a scatter plot to just see how the data is distributed differently

gss_2018_tbl %>%  #same comments will not be restated
  ggplot(aes(NUMPETS,INCOME))+ 
  geom_jitter()+ #removed the specifications on jitter because it was too cramped and this looks better
  geom_smooth(method="lm") 

gss_2018_tbl %>%  #same comments will not be restated
  ggplot(aes(PARSOL,INCOME))+ 
  geom_jitter(width=0.3, height = 0.3)+ 
  geom_smooth(method="lm") 

gss_2018_tbl %>%  #same comments will not be restated
  ggplot(aes(AGE,INCOME))+ 
  geom_jitter(width=0.3, height = 0.3)+ 
  geom_smooth(method="lm") 

gss_2018_tbl %>%
  ggplot(aes(ASTROSCI,INCOME))+
  geom_boxplot()


#Analysis

#Hypothesis 1, are income and age correlated? 
cor.test(gss_2018_tbl$INCOME,gss_2018_tbl$AGE) #used cor.test instead of cor because cor.test just gives me all of the statistics I would need to report here, p, t and correlations, for less effort.


#Hypothesis 2, Is income different based on how scientific you believe astrology to be, and if yes, which levels?
model0<- aov(INCOME ~ASTROSCI,data=gss_2018_tbl)
summary(model0)
TukeyHSD(model0)


#RQ 1 /Hypothesis or RQ 3
# Reduced model
holdout_indices <- createDataPartition(gss_2018_tbl$INCOME, #making a holdout_indices to split the data easily, used createdatapartition because it greate at making test/training partitions, better than other options like createResample, that's better for making bootstrap samples.
                                       p = .25, #Split the test group to 25% of my total data
                                       list = T)$Resample1 #List = T makes thing easier to pull, like Resample1, which is what we pulled here.
test_tbl <- gss_2018_tbl[holdout_indices,] #created test dataset and training dataset in easiest way I know.
training_tbl <- gss_2018_tbl[-holdout_indices,]

training_folds <- createFolds(training_tbl$INCOME) #Used createFold because it's a convenient way of splitting the data into "k" (default value is 10) groups, which I will need for the k-fold (this case 10 fold) CV

model1 <- train(
  INCOME ~ ., #try to predict income from the 3 other items in my dumb model
  training_tbl, #use the training data set so I can compare later with holdout CV
  method="glmnet", #using Elastic net (thought it would be better than LM and you wanted us to test 3 model so I used glmnet as the third, because I really care about the random forest and eXtreme Gradient Boosting)
  na.action = na.pass, #specify what the model should do with NAs. didn't leave the default because I want the model to run. Didn't do na.omit because I want to keep my sample size large enough.
  preProcess = c("center","scale","zv","nzv","medianImpute"), #zv and nzv find columns with either zero variance or close to zero variance and excludes them (kept them for when I use the model), "center" centers the data (subtracts the mean from the values), while "scale" standardizes it by dividing it by the sd. medianImpute calculates the median.
  trControl = trainControl(method="cv", #specify the type of resampling
                           number=10, #Specify the number of folds 
                           verboseIter=T, #Prints more information and me want see more
                           indexOut = training_folds) #mainly to keep my folds consistent across models
)
model1
cv_m1 <- max(model1$results$Rsquared) #pulled the largest r squared from the model (this is the k fold cv r squared)
holdout_m1 <- cor( #Calculating the correlations between what our model predicts the test/holdout sample would be with the actual values in that sample), then squaring that correlation to get the holdout CV r squared
  predict(model1, test_tbl, na.action = na.pass),
  test_tbl$INCOME
)^2


model2 <- train( #same comments as before, will not repeat
  INCOME ~ .,
  training_tbl,
  method="ranger",
  na.action = na.pass,
  preProcess = c("center","scale","zv","nzv","medianImpute"),
  trControl = trainControl(method="cv", 
                           number=10, 
                           verboseIter=T, 
                           indexOut = training_folds)
)
model2
cv_m2 <- max(model2$results$Rsquared) 
holdout_m2 <- cor( 
  predict(model2, test_tbl, na.action = na.pass),
  test_tbl$INCOME
)^2


model3 <- train( #same comments as before, will not repeat
  INCOME ~ .,
  training_tbl,
  method="xgbLinear",
  na.action = na.pass,
  preProcess = c("center","scale","zv","nzv","medianImpute"),
  trControl = trainControl(method="cv", 
                           number=10, 
                           verboseIter=T, 
                           indexOut = training_folds)
)
model3
cv_m3 <- max(model3$results$Rsquared) 
holdout_m3 <- cor( 
  predict(model3, test_tbl, na.action = na.pass),
  test_tbl$INCOME
)^2

# Full model
#same code as previous, comments will be removed other than new added code

local_cluster <- makeCluster(detectCores() - 1) # getting number of cores
registerDoParallel(local_cluster) # clustering
#this is the most efficient way to parallalize things as far as I'm aware so I'm doing it.
#I'm parallalizing to save time mainly

holdout_indices_2 <- createDataPartition(gss_2018_tbl_2$INCOME, 
                                       p = .25, 
                                       list = T)$Resample1 
test_tbl_2 <- gss_2018_tbl_2[holdout_indices_2,] 
training_tbl_2 <- gss_2018_tbl_2[-holdout_indices_2,]

training_folds_2 <- createFolds(training_tbl_2$INCOME) 
model4 <- train(
  INCOME ~ ., 
  training_tbl_2, 
  method="glmnet", 
  na.action = na.pass, 
  preProcess = c("center","scale","zv","nzv","medianImpute"), 
  trControl = trainControl(method="cv", 
                           number=10,  
                           verboseIter=T, 
                           indexOut = training_folds_2) 
)
model4
cv_m4 <- max(model4$results$Rsquared) 
holdout_m4 <- cor( 
  predict(model4, test_tbl_2, na.action = na.pass),
  test_tbl_2$INCOME
)^2

model5 <- train( 
  INCOME ~ .,
  training_tbl_2,
  method="ranger",
  na.action = na.pass,
  preProcess = c("center","scale","zv","nzv","medianImpute"),
  trControl = trainControl(method="cv", 
                           number=10, 
                           verboseIter=T, 
                           indexOut = training_folds_2)
) #Got the following error :In nominalTrainWorkflow(x = x, y = y, wts = weights, info = trainInfo,  :There were missing values in resampled performance measures.
#choosing to ignore this message because it's just saying that there are some missing values and I know that
model5
cv_m5 <- max(model5$results$Rsquared) 
holdout_m5 <- cor( 
  predict(model5, test_tbl_2, na.action = na.pass),
  test_tbl_2$INCOME
)^2

model6 <- train(
  INCOME ~ .,
  training_tbl_2,
  method="xgbLinear",
  na.action = na.pass,
  preProcess = c("center","scale","zv","nzv","medianImpute"),
  trControl = trainControl(method="cv", 
                           number=10, 
                           verboseIter=T, 
                           indexOut = training_folds_2)
)
model6
cv_m6 <- max(model6$results$Rsquared) 
holdout_m6 <- cor( 
  predict(model6, test_tbl_2, na.action = na.pass),
  test_tbl_2$INCOME
)^2

stopCluster(local_cluster) 
registerDoSEQ()
#These two lines are to stop parallalization



#Publication

table_1 <- data.frame( #Chose to use data.frame instead of tibble, because the default display shows to more decimal places and the cv_rqs_full was 0.9997829 and was just displaying as 1 when I used tibble.
  algo = c("elastic net","random forests","xgboost"), # just filling in the table, will not comment on most of these
  cv_rqs_reduced = c(
    cv_m1,
    cv_m2,
    cv_m3
  ),
  ho_rqs_reduced = c(
    holdout_m1,
    holdout_m2,
    holdout_m3
  ),
  cv_rqs_full=c(
    cv_m4,
    cv_m5,
    cv_m6),
  ho_rqs_full=c(
    holdout_m4,
    holdout_m5,
    holdout_m6
  )
)
write_csv(table_1, file="../out/model_comp.csv") #used write_csv instead of RDS, because it's a table and this is the final version of it, never to be used again, so I wanted it to be a csv file that's dumpped into out.


#Data Export
gss_2018_tbl %>%
  mutate(AGE_25= AGE>=25)%>% #made a new variable that has TRUE for when the age is 25 or older to reduce processing time when I run the app.
  saveRDS("../shiny/shiny_final/import.RDS") #exported as rds because it's the best file to export for R, won't go through any shenanigans of having a different exporting or importing of different data types
