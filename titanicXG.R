##TITANIC KAGGLE COMPETITION!!!
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library(xgboost)
library(rpart)
library(Ckmeans.1d.dp)
# Set dir
setwd("/Users/davidegbert/Rprojects/titanic")

#Load in data
train <- read.csv("./data/train.csv", stringsAsFactors = FALSE)
test <- read.csv("./data/test.csv", stringsAsFactors = FALSE)

full <- bind_rows(train, test)

View(train)


full <- full %>%
  mutate(Title = (gsub('(.*, )|(\\..*)', '', Name)),
         Surname = (gsub('(,)(.)*', '', Name)),
         Fsize = (SibSp + Parch + 1),
         Family = paste(Surname, Fsize, sep = '_')
         )



# Titles with very low cell counts to be combined to "rare" level
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

# Also reassign mlle, ms, and mme accordingly
full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% rare_title]  <- 'Rare Title'

# Show title counts by sex again
table(full$Sex, full$Title)

full %>%
  filter(!is.na(Survived)) %>%
  ggplot(mapping = aes(x = Fsize, fill = factor(Survived))) +
    geom_bar(stat = "count", position = "dodge") +
    scale_x_continuous(breaks = c(1:11)) +
    labs(x = "Family Size")

# Discretize family size
full$FsizeD[full$Fsize == 1] <- 'singleton'
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- 'small'
full$FsizeD[full$Fsize > 4] <- 'large'

# Show family size by survival using a mosaic plot
mosaicplot(table(full$FsizeD, full$Survived), main='Family Size by Survival', shade=TRUE)

View(full)

# Create a deck variable. Going to use dplyr and stringr's str_sub function

full <- full %>%
  mutate(Deck = str_sub(Cabin, 1, 1))

View(full)

## Imputation not Amputation!!!

## Passengers 62 and 830 are missing the Embarkment information. They are in the same cabin, are both singletons and paid the same fare.

# Get rid of our missing passenger IDs
embark_fare <- full %>%
  filter(PassengerId != 62 & PassengerId != 830)

ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()

## Median for C (Charbourg) coincedes with $80 - the same that our single ladies paid. So let's add C to their observation

full$Embarked[c(62, 830)] <- 'C'

# Passenger ID 1044 does not have a fare. We do know he is Pclass 3 and Embarked Southampton

full %>%
  filter(Pclass == 3 & Embarked == 'S') %>%
  ggplot(aes(x = Fare)) +
  geom_density(fill = '#99d6ff', alpha = 0.4) +
  geom_vline(aes(xintercept = median(Fare, na.rm = TRUE)),
             color = 'red', linetype = 'dashed', lwd = 1, show.legend = TRUE
             ) +
  scale_x_continuous(labels = dollar_format()) +
  theme_few()

full %>%
  filter(Pclass == 3 & Embarked == 'S') %>%
  summarize(median(Fare, na.rm = TRUE))

## So we'll use $8.05, the median as the fare for Mr. Storey aka #1044

full$Fare[1044] <- 8.05

## There are a bunch of missing ages. How many? Let's see...

sum(is.na(full$Age)) 

# 263

# predictive imputation using mice

# Make variables factors into factors
factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
                 'Title','Surname','Family','FsizeD')

full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))

# Set a random seed
set.seed(129)

# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(full[, !names(full) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method = 'rf')  

mice_output <- mice::complete(mice_mod)

# Plot age distributions
par(mfrow = c(1,2))
hist(full$Age, freq = F, main = 'Age: Original Data', 
     col = 'darkgreen', ylim = c(0,0.04))
hist(mice_output$Age, freq = F, main = 'Age: MICE Output', 
     col = 'lightgreen', ylim = c(0,0.04))

# Replace Age variable from the mice model.
full$Age <- mice_output$Age

# Show new number of missing Age values
sum(is.na(full$Age))

# First we'll look at the relationship between age & survival
ggplot(full[1:891,], aes(Age, fill = factor(Survived))) + 
  geom_histogram() + 
  # I include Sex since we know (a priori) it's a significant predictor
  facet_grid(.~Sex) + 
  theme_few()

# Create the column child, and indicate whether child or adult
full$Child[full$Age < 9] <- 'Child'
full$Child[full$Age >= 9] <- 'Adult'

# Show counts
table(full$Child, full$Survived)

# Adding Mother variable
full$Mother <- 'Not Mother'
full$Mother[full$Sex == 'female' & full$Parch > 0 & full$Age > 18 & full$Title != 'Miss'] <- 'Mother'

# Show counts
table(full$Mother, full$Survived)

full$Child  <- factor(full$Child)
full$Mother <- factor(full$Mother)

md.pattern(full)

## Time to model - Let's give XGBoost a shot...

# Dataframe with only the variables I will use in the model

xg.full <- full %>%
  select(c(Survived, Pclass , Sex , Age , SibSp , Parch , 
             Fare , Embarked , Title , 
             FsizeD , Child , Mother))
str(xg.full)

#Turn factors into numerics and start at 0 (XGBoost req)
xg.full$Survived <- as.numeric(xg.full$Survived)
xg.full$Pclass <- as.numeric(xg.full$Pclass) - 1
xg.full$Sex <- as.numeric(xg.full$Sex) - 1
xg.full$Embarked <- as.numeric(xg.full$Embarked) - 1
xg.full$Title <- as.numeric(xg.full$Title) - 1
xg.full$FsizeD <- as.numeric(xg.full$FsizeD) - 1
xg.full$Child <- as.numeric(xg.full$Child) - 1
xg.full$Mother <- as.numeric(xg.full$Mother) - 1
xg.full$SibSp <- as.numeric(xg.full$SibSp)
xg.full$Parch <- as.numeric(xg.full$Parch)
str(xg.full)

# Convert to a matrix

xg.full <- as.matrix(xg.full)

# Splitting back to train and test sets
train <- xg.full[1:891,]
test <- xg.full[892:1309,]

bst <- xgboost(data = train[, -c(1)], label = train[, c(1)],  nthread = 2, nround = 15, objective = "binary:logistic", verbose = 1)

# Get the feature real names
names <- dimnames(train)[[2]]

# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = bst)

# Plotting
xgb.plot.importance(importance_matrix)

# Prediction on test and train sets
pred_xgboost_test <- predict(bst, test[, -c(1)])
pred_xgboost_train <- predict(bst, train[, -c(1)])


err <- mean(as.numeric(pred_xgboost_train > 0.45) != train[, c(1)])
print(paste("test-error=", err))

prediction <- as.numeric(pred_xgboost_test > 0.45)
prediction
xg.submission <- as.data.frame(prediction)
xg.submission <- cbind(full$PassengerId[892:1309], xg.submission)
View(xg.submission)
xg.submission <- data.frame(PassengerId = xg.submission$`full$PassengerId[892:1309]`, Survived = xg.submission$prediction)

write.csv(xg.submission, file = 'xg.mod.Solution1.csv', row.names = F)
