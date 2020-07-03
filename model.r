library(tidyverse)
library(dslabs)
library(caret)
library(randomForest)

mnist <- read_mnist()
set.seed(42)

set.seed(1990)
index <- sample(nrow(mnist$train$images), 50000)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])

index <- sample(nrow(mnist$test$images), 5000)
x_test <- mnist$test$images[index,]
y_test <- factor(mnist$test$labels[index])

nvz <- nearZeroVar(x)
col_index <- setdiff(1:ncol(x), nvz)

colnames(x) <- 1:ncol(mnist$train$images)
colnames(x_test) <- colnames(x)

control <- trainControl(method="cv", number = 5)
grid <- data.frame(mtry = c(1, 5, 10, 25, 50, 100))

train_rf <-  train(x[, col_index], y, 
                   method = "rf", 
                   ntree = 150,
                   trControl = control,
                   tuneGrid = grid,
                   nSamp = 5000)

y_hat_rf <- predict(fit_rf, x_test[ ,col_index])
cm <- confusionMatrix(y_hat_rf, y_test)
cm$overall["Accuracy"]

