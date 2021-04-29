##### Cardiovascular Disease Prediction using Logistic Regression #######
##### Author : SD ############
##### Last Update Date : 3rd July 2019 ########

############## Importing CVD Dataset ########################
getwd() # prints the working directory
# Importing Heart Disease data in R dataframe
df_hdsource <- read.csv(file ='/Users/Shared/cvdpreddataset/cleveland.csv', 
               header = FALSE, sep = ",", quote = "",
               dec = ".", fill = TRUE, comment.char = "")

# Adding Header to the dataframe
names(df_hdsource) <- c('Age', 'Gender', 'CP', 'Trestbps', 'Chol', 'FBS', 'RestECG', 'Thalach', 
               'Exang', 'Oldpeak', 'Slope', 'CA', 'Thal', 'Num')

# Checking the dimension and structure of the original dataframe
dim(df_hdsource)
str(df_hdsource)

# Check sample records
head(df_hdsource, 10)


################# Installing Required Libraries ########################
# Checks list of installed packages/libraries. 
# and installs and loads the missing ones into the R session.

#Package definition
utilpack <- function(pkgs){
  new.pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
  if (length(new.pkgs)) 
    install.packages(new.pkgs, dependencies = TRUE)
  sapply(pkgs, require, character.only = TRUE)
}

# Calling utilpack with parameter list as 
packages <- c("ggplot2", "knitr", "mlr", "tidyverse", "GGally", "cowplot", "dplyr", 
              "tidyr","readr", "magrittr", "moments", "ggthemes", 'psych', "broom", "survey", "lmtest","caret")
utilpack(packages)

# Preliminary analysis of original dataframe
describe(df_hdsource)
summary(df_hdsource)
summarizeColumns(df_hdsource) %>% knitr::kable( caption =  'Feature Summary of Source Data File')

################# Data Cleansing Tasks #######################
#Creating new dataframe for mutating and refactoring
df_clean <- df_hdsource

# Use the 'mutate' function from dplyr to re-code Num and create a new categorical variable HeartDisease
df_clean <- df_clean %>% mutate(HeartDisease = ifelse(Num > 0, 1, 0))

# Re-code HeartDisease using mutate function. Provide descriptive names instead of numbers
df_clean <- df_clean %>% mutate(HeartDisease = factor(HeartDisease, 
                                                      levels = 0:1, 
                                                      labels = c("Healthy", "Heart Disease")
)
)

#Dropping column Num
df_clean <- df_clean[, -c(14)]

# Re-code Gender using mutate function. Provide descriptive names instead of numbers
df_clean <- df_clean %>% mutate(Gender = factor(Gender, 
                                                levels = 0:1, 
                                                labels = c("Female", "Male")
)
)
# Re-code CP using mutate function. Provide descriptive names instead of numbers
df_clean <- df_clean %>% mutate(CP = factor(CP, 
                                            levels = 1:4, 
                                            labels = c("Typical Angina", "Atypical Angina", "Non-anginal Pain", "Asymptom") 
                                            
)
)
# Re-code FBS using mutate function. Provide descriptive names instead of numbers
df_clean <- df_clean %>% mutate(FBS = factor(FBS, 
                                             levels = 0:1, 
                                             labels = c("FBS <=120", "FBS > 120")
)
)
# Re-code RestECG using mutate function. Provide descriptive names instead of numbers
df_clean <- df_clean %>% mutate(RestECG = factor(RestECG, 
                                                 levels = 0:2, 
                                                 labels = c("RestECG-0", "RestECG-1", "RestECG-2")
)
)
# Re-code Exang using mutate function. Provide descriptive names instead of numbers
df_clean <- df_clean %>% mutate(Exang = factor(Exang, 
                                               levels = 0:1, 
                                               labels = c("No", "Yes")
)
)
# Re-code Slope using mutate function. Provide descriptive names instead of numbers
df_clean <- df_clean %>% mutate(Slope = factor(Slope, 
                                               levels = 1:3, 
                                               labels = c("Peak Exercise- Upslope", "Peak Exercise- Flat", "Peak Exercise- Downslope")
)
)

#Removing NULL values from CA column
df_clean <- df_clean[df_clean$CA != "?", ]
#Refactoring CA column
df_clean$CA <- factor(df_clean$CA)

#Removing NULL values from Thal column
df_clean <- df_clean[df_clean$Thal != "?", ]
#Refactoring Thal column
df_clean$Thal <- factor(df_clean$Thal)
# Renaming the levels of Thal
levels(df_clean$Thal) <- c("Normal", "Fixed Defect", "Reversible Defect")

################# Descriptive statistics on clean dataset ####################### 

#Dimension of the clean dataframe
dim(df_clean) 
str(df_clean)
#Summary statistics of the cleandataframe
summary(df_clean) 
summarizeColumns(df_clean) %>% knitr::kable( caption =  'Feature Summary: Cleaned, transformed data')
#Descriptive statistics of the clean dataframe
desc_stats <- describe(df_clean)


######## Visual Statistics ##########

# Barplot:Distribution of Heart Disease Dataset
ggplot(df_clean,aes(HeartDisease, fill=HeartDisease)) +
  geom_bar(stat="count") +
  theme_fivethirtyeight()  +
  scale_fill_manual(values=c("darkseagreen","indianred3")) +
  ggtitle("Distribution Heart Disease Dataset") +
  ylab("Count")
# use colors "grey" and "grey47" for greyscale effects. 

# Boxplot:Distribution of Patient Age by Disease Category 
ggplot(df_clean, aes(x=HeartDisease, y=Age, fill=HeartDisease)) +
  geom_boxplot(notch = T, color="grey30") +
  theme_fivethirtyeight()  +
  scale_fill_manual(values = c("darkseagreen", "indianred3")) +
  ggtitle("Dist. of Age by Disease Category")

# Barplot:Distribution of Gender by Disease Category
ggplot(df_clean,aes(HeartDisease, fill=HeartDisease)) +
  geom_bar(stat="count") +
  facet_wrap(~Gender, ncol=2,scale="fixed") +
  theme_fivethirtyeight()  +
  scale_fill_manual(values=c("darkseagreen","indianred3")) +
  xlab("Gender") +
  ylab("Count") +
  ggtitle("Dist. of Gender by Disease Category") 

# Barplot:Distribution of CP Type by Disease Category
ggplot(df_clean,aes(HeartDisease, fill=HeartDisease)) +
  geom_bar(stat="count") +
  facet_wrap(~CP, ncol=2,scale="fixed") +
  theme_fivethirtyeight()  +
  scale_fill_manual(values=c("darkseagreen","indianred3")) +
  xlab("Chest Pain Type") +
  ylab("Count") +
  ggtitle("Dist. of CP by Disease Category") 

# Boxplot:Distribution Trestbps by Disease Category
ggplot(df_clean, aes(x=HeartDisease, y=Trestbps, fill=HeartDisease)) +
  geom_boxplot(notch = T, color="grey30") +
  theme_fivethirtyeight()  +
  scale_fill_manual(values = c("darkseagreen", "indianred3")) +
  ggtitle("Dist. of Trestbps by Disease Category")

# Boxplot:Distribution Cholestoral by Disease Category
ggplot(df_clean, aes(x=HeartDisease, y=Chol, fill=HeartDisease)) +
  geom_boxplot(notch = T, color="grey30") +
  theme_fivethirtyeight()  +
  scale_fill_manual(values = c("darkseagreen", "indianred3")) +
  ggtitle("Dist. of Chol by Disease Category")

# Barplot: Distribution of FBS by Disease Category
ggplot(df_clean,aes(HeartDisease, fill=HeartDisease)) +
  geom_bar(stat="count") +
  facet_wrap(~FBS, ncol=2,scale="fixed") +
  theme_fivethirtyeight()  +
  scale_fill_manual(values=c("darkseagreen","indianred3")) +
  ylab("Count")+
  ggtitle("Dist. of FBS by Disease Category")

# Barplot: Distribution of RestECG by Disease Category
ggplot(df_clean,aes(HeartDisease, fill=HeartDisease)) +
  geom_bar(stat="count") +
  facet_wrap(~RestECG, ncol=2,scale="fixed") +
  theme_fivethirtyeight()  +
  scale_fill_manual(values=c("darkseagreen","indianred3")) +
  ylab("Count")+
  ggtitle("Dist. of RestECG by Disease Category")

# Boxplot:Distribution Thalach by Disease Category
ggplot(df_clean, aes(x=HeartDisease, y=Thalach, fill=HeartDisease)) +
  geom_boxplot(notch = T, color="grey30") +
  theme_fivethirtyeight()  +
  scale_fill_manual(values = c("darkseagreen", "indianred3")) +
  ggtitle("Dist. of Thalach by Disease Category")

# Barplot: Distribution of Exang by Disease Category
ggplot(df_clean,aes(HeartDisease, fill=HeartDisease)) +
  geom_bar(stat="count") +
  facet_wrap(~Exang, ncol=2,scale="fixed") +
  theme_fivethirtyeight()  +
  scale_fill_manual(values=c("darkseagreen","indianred3")) +
  ylab("Count")+
  ggtitle("Dist. of Exang by Disease Category")

# Boxplot:Distribution Oldpeak by Disease Category
ggplot(df_clean, aes(x=HeartDisease, y=Oldpeak, fill=HeartDisease)) +
  geom_boxplot(notch = T, color="grey30") +
  theme_fivethirtyeight()  +
  scale_fill_manual(values = c("darkseagreen", "indianred3")) +
  ggtitle("Dist. of Oldpeak by Disease Category")

# Barplot: Distribution of Slope by Disease Category
ggplot(df_clean,aes(HeartDisease, fill=HeartDisease)) +
  geom_bar(stat="count") +
  facet_wrap(~Slope, ncol=2,scale="fixed") +
  theme_fivethirtyeight()  +
  scale_fill_manual(values=c("darkseagreen","indianred3")) +
  ylab("Count")+
  ggtitle("Dist.of Slope by Disease Category")

# Barplot: Distribution of CA by Disease Category
ggplot(df_clean,aes(HeartDisease, fill=HeartDisease)) +
  geom_bar(stat="count") +
  facet_wrap(~CA, ncol=2,scale="fixed") +
  theme_fivethirtyeight()  +
  scale_fill_manual(values=c("darkseagreen","indianred3")) +
  ylab("Count")+
  ggtitle("Dist. of CA by Disease Category")

# Barplot: Distribution of Thal by Disease Category
ggplot(df_clean,aes(HeartDisease, fill=HeartDisease)) +
  geom_bar(stat="count") +
  facet_wrap(~Thal, ncol=2,scale="fixed") +
  theme_fivethirtyeight()  +
  scale_fill_manual(values=c("darkseagreen","indianred3")) +
  ylab("Count")+
  ggtitle("Dist. of Thal by Disease Category")


################### Variable Selection #########################
# Run 1st GLM- Logistic Regression on all variables
glm_fullmodel<-glm(HeartDisease~., data=df_clean, family=binomial(link = 'logit'))
summary(glm_fullmodel)
log.glm_fullmodel <- tidy(glm_fullmodel)

# Checking the variable of importance
varImp(glm_fullmodel)

# Statistical Tests for Individual Predictors--Wald Test
regTermTest(glm_fullmodel, "Age")
regTermTest(glm_fullmodel, "Gender")
regTermTest(glm_fullmodel, "CP")
regTermTest(glm_fullmodel, "Trestbps")
regTermTest(glm_fullmodel, "Chol")
regTermTest(glm_fullmodel, "FBS")
regTermTest(glm_fullmodel, "RestECG")
regTermTest(glm_fullmodel, "Thalach")
regTermTest(glm_fullmodel, "Exang")
regTermTest(glm_fullmodel, "Oldpeak")
regTermTest(glm_fullmodel, "Slope")
regTermTest(glm_fullmodel, "CA")
regTermTest(glm_fullmodel, "Thal")


# Variable Selection for Model Building
# We can see that only the following are significant parameters that may have effect on Heart Disease:
#Gender(2), CP(3), CA(12), Thal(13) and the Outcome variable - HeartDisease(14)

# Keep those and remove others

df_clean2<-df_clean[,c(2,3,12,13,14)]
summary(df_clean2)
summarizeColumns(df_clean2) %>% knitr::kable( caption =  'Feature Summary: Selected variables for New Model')
desc_stats_df_clean2 <- describe(df_clean2)

# Run 2nd GLM - Logistic Regression on selected variables
glm_mod2 <- glm(HeartDisease~., data=df_clean2, family=binomial(link = 'logit'))
summary(glm_mod2)
log.glm_mod2 <- tidy(glm_mod2)


############## Hypothesis Testing #################################

# Hypothesis Test for Individual Predictors--Wald Test
regTermTest(glm_mod2, "Gender")
regTermTest(glm_mod2, "CP")
regTermTest(glm_mod2, "CA")
regTermTest(glm_mod2, "Thal")


# Hypothesis Test for Logistic Regression Model - Likelihood Ratio Test
# Anova function with LRT or lrtest function can be used 
anova(glm_fullmodel, glm_mod2, test ="LRT")
lrtest(glm_fullmodel, glm_mod2)

# Plot
plot(fitted(glm_mod2), 
     rstandard(glm_mod2))

# Geom Bar Comparison
log.glm_mod2 %>%
  mutate(term=reorder(term,estimate)) %>%
  ggplot( aes(term,estimate, fill=estimate)) +
  geom_bar(stat="identity") +
  scale_fill_gradient(low="darkseagreen", high="indianred3") +
  theme_fivethirtyeight()  +
  geom_hline(yintercept=0) +
  coord_flip()+
  ggtitle("GLM - Estimate Plot")


############### Model Building ################
########## Creating Train and Test Dataset ############
# Create Training Model Dataframe from 2nd GLM dataframe
df_hddata<-df_clean2

# Set Seed Value
set.seed(999)

# Create training data partition. Set to 75% for training data and 25% for testing.
df_train <- createDataPartition(
  y = df_hddata$HeartDisease,
  p = .75,
  list = FALSE
)
str(df_train)

# Creating training dataset - 75%
df_TrainSet <- df_hddata[df_train, ]

# Creating testing/validation data set - 25%
df_ValidSet <- df_hddata[-df_train,]

# Checking the number of rows in the dataset
nrow(df_TrainSet)
nrow(df_ValidSet)

############### Model Building ################
############### Model Training ################

# Tuning parameters for the model using trainControl from caret package.
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)


df_TrainSet$HeartDisease<-make.names(df_TrainSet$HeartDisease)
set.seed(139)
df_TrainSet$HeartDisease<-as.factor(df_TrainSet$HeartDisease)
head(df_TrainSet, 10)

# Logistic Regression with the train function in caret package
hddata_fit <- train(HeartDisease ~ ., 
                    data = df_TrainSet ,
                    method = "glm", 
                    trControl = fitControl,
                    metric="ROC")

hddata_fit # ROC, Sensitivity, Specificity

# Checking the Variable of Importance in the trained model
varImp(hddata_fit)

############## Model Building ################
############## Model Testing #################
# Run predictive analytics on the trained model using test dataset

hd_pred <- predict(hddata_fit,df_ValidSet)
levels(hd_pred)[2] <- "Heart Disease"
hd_table<-table(hd_pred, df_ValidSet$HeartDisease)
hd_table.df<-as.data.frame(hd_table)
hd_pred_result<-confusionMatrix(hd_table, positive="Heart Disease")
hd_pred_result # Accuracy, Sensitivity, Specificity, Kappa, p-Value 


# Plotting the Confusion Matrix for Logistic Regression
ggplot(data = hd_table.df, aes(x = Var2, y = hd_pred, label=Freq)) +
  geom_tile(aes(fill = Freq)) +
  scale_fill_gradient(low="indianred3", high="darkseagreen") +
  theme_fivethirtyeight()  +
  xlab("Actual Heart Disease") +
  ylab("Predicted Heart Disease") +
  geom_text(size=8) +
  ggtitle("Confusion Matrix for CVD using LR")

)