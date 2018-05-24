#**************************************************************
# Classify the disease with metagenomics data
#**************************************************************
# load data
dir <- 'metagenomics/febrile.data'
febrile <- read.table(dir, header=TRUE, sep=',')

# max-min Normalization
normalize <- function(x) {
    num <- x - min(x)
    denom <- max(x) - min(x)
    return (num/denom)
}

febrile.norm <- as.data.frame(lapply(febrile[2:length(febrile)], normalize))
febrile.norm$Diagnosis <- febrile$Type
str(febrile.norm)

# divide train set and test set at ratio 0.8
set.seed(3)
# train set
train_id <- sample(1:length(febrile.norm$Diagnosis), length(febrile.norm$Diagnosis)*0.8)
train <- febrile.norm[train_id,]
train_labels <- train$Diagnosis
train <- febrile.norm[train_id, -length(febrile.norm)]
summary(train)
# test set
test <- febrile.norm[-train_id,]
test_labels <- test$Diagnosis
test <- febrile.norm[-train_id, -length(febrile.norm)]
summary(test)

# k-folder crossvalidate
library(caret)
train_control <- trainControl(method="repeatedcv", number=7, repeats=3)
model_nb <- train(train, train_labels, trControl=train_control, method='nb')
print(model_nb)

# evaluate the model
library(gmodels)
predicts <- predict(model_nb, test)
CrossTable(x=test_labels, y=predicts, prop.chisq=F)
