#Take home exam Big Data Econometrics
#Leonardo Galassi

rm(list = ls())

library(readxl)
library(DataExplorer)
library(dplyr)
library(MASS)
library(broom)
library(effects)

# open the dataset
file_path<- "C:/Users/galas/OneDrive - Università Politecnica delle Marche/Desktop/Big Data Econometrics/lfp.csv"

db<- read.csv(file_path)

# a little exploratory analysis
summary(db)
head(db)
tail(db)
str(db)

# Logistic regression

# with all variables
RegLogAll<- glm(LFP ~ ., data = db, family = binomial)
summary(RegLogAll)  #it makes little sense due to some variables

# Correlation analysis
plot_correlation(db)

# dataset cleaning
db_clean <- db[,-c(1,3,5,6,9,10,11,12,13,15,16,17,18,25)]
summary(db_clean)
plot_correlation(db_clean)

# Convert Variables
db_clean$SEX <- as.factor(db_clean$SEX)
db_clean$LFP <- as.factor(db_clean$LFP)
db_clean$STACIV <- as.factor(db_clean$STACIV)
db_clean$ETA <- as.numeric(db_clean$ETA)

# Reg Log with dataset cleaning
RegLog <- glm(LFP ~ ., data = db_clean, family = binomial)
summary(RegLog)

# Stepwise regression
modello_stepwise <- step(RegLog, direction = "backward")
summary(modello_stepwise)


model_tidy <- tidy(modello_stepwise)
print(model_tidy)


# Split dataset into male and female using subset()
male_db <- subset(db_clean, SEX == 1)
female_db <- subset(db_clean, SEX == 2)


## Delete of the variable SEX to avoid redundancy
male_db <- male_db[,-c(2)]
female_db <- female_db[,-c(2)]


### Male glm
male_model <- glm(LFP ~ ., data = male_db, family = binomial)
summary(male_model)


stepwise_male <- step(male_model, direction = "backward")
summary(stepwise_male)


### Female glm
female_model <- glm(LFP ~ ., data = female_db, family = binomial)
summary(female_model)


stepwise_female <- step(female_model, direction = "backward")
summary(stepwise_female)



# Adding an interaction between "SEX" and all variables in the full model
interaction_model <- glm(LFP ~ . + SEX*NCOMP+SEX*STACIV+SEX*ETA
                         +SEX*STUDIO+SEX*PERC+SEX*PERL+SEX*NPERL+
                         SEX*NPERC+SEX*AREA5+SEX*ACOM4C, data = db_clean, family = binomial)

summary(interaction_model)
