#Run once to install packages.
#install.packages("rpart.plot")
library("rpart")
library("rpart.plot")	

#set working directory to current directory. 
setwd("~/githubdesktop/0_class/techfundamentals-fall2017-materials/classes/10-intro-modeling2")
#Load the Titanic Dataset
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")
test$Survived <- 0

### CLEANING DATA   ###
#Here we are combining the training and test data with rowbind. 
combi <- rbind(train, test)
combi$Name <- as.character(combi$Name)

#We are going to do some feature creation for "Title"
#This gives us the Split name. 
strsplit(combi$Name[1], split='[,.]')

#This function gives us the title. 
strsplit(combi$Name[1], split='[,.]')[[1]][2]


#This will dump the string split into a new version. 
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})

#This will clean up to remove the spaces.
combi$Title <- sub(' ', '', combi$Title)

#Show all of the titles
table(combi$Title)

#Recode the data based on Infrequent classes. 
combi$Title[combi$Title %in% c('Lady', 'the Countess', 'Mlle', 'Mee', 'Ms')] <- 'Miss'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir', 'Col', 'Jonkheer', 'Rev', 'Dr', 'Master')] <- 'Mr'
combi$Title[combi$Title %in% c('Dona','Mme')] <- 'Mrs'

#change from a text to a factor variable so that R will encode. 
combi$Title <- factor(combi$Title)
combi$Title

#Show all of the Embarked
table(combi$Embarked)

#Looks like we have 2 that are blanks, not NA. Manual recoding. 
combi$Embarked[c(62,830)] <- "S"

# Passenger on row 1044 has an NA Fare value. Let's replace it with the median fare value.
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)

# Create new column -> family_size
combi$family_size <- combi$SibSp + combi$Parch + 1

# How to fill in missing Age values?
# We make a prediction of a passengers Age using the other variables and a decision tree model. 
# This time you give method="anova" since you are predicting a continuous variable.

predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + family_size,
                       data=combi[!is.na(combi$Age),], method="anova")
combi$Age[is.na(combi$Age)] <- predict(predicted_age, combi[is.na(combi$Age),])


#Let's include the cabin.
table(combi$Cabin)

#Grab the first letter.
combi$Cabin <- substr(combi$Cabin,1,1)
table(combi$Cabin)
#Get Rid of Blanks
combi$Cabin[combi$Cabin == ""] <- "H"
combi$Cabin[combi$Cabin == "T"] <- "H"
table(combi$Cabin)

### Respliting data for processing ###
train_new <- combi[1:891,]
test_new <- combi[892:1309,]
test_new$Survived <- NULL

# train_new and test_new are available in the workspace
str(train_new)
str(test_new)

# Create a simple model `my_tree`
my_tree1 <- rpart(Survived ~ Age + Sex, data = train_new, method = "class", control=rpart.control(cp=0.1))
summary(my_tree1)
prp(my_tree1, type = 4, extra = 100)

my_tree1 <- rpart(Survived ~ Age + Sex, data = train_new, method = "class", control=rpart.control(cp=0.000001))
summary(my_tree1)
prp(my_tree1, type = 4, extra = 100)

# Create a new model `my_tree`
my_tree2 <- rpart(Survived ~ Age + Sex + Pclass  + family_size, data = train_new, method = "class", control=rpart.control(cp=0.001))
summary(my_tree2)
prp(my_tree2, type = 4, extra = 100)

#Logistic Regression
logmodel<-glm(Survived ~  Age + Sex + Pclass  + family_size, data = train_new, family = "binomial")
summary(logmodel)

#Train Model 
train_new$Survived_log <- ifelse(predict(logmodel, train_new, type="response")>0.5,1,0)
#Predict for Test
test_new$Survived_log <- ifelse(predict(logmodel, test_new, type="response")>0.5,1,0)

setwd("~/githubdesktop/0_class/techfundamentals-fall2017-materials/classes/input")
write.csv(test_new, file = "train_new.csv",row.names=FALSE)
write.csv(train_new, file = "test_new.csv",row.names=FALSE)
