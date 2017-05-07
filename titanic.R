##TITANIC KAGGLE COMPETITION!!!
library('ggplot2') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm

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
