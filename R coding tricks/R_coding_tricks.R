# assigning and printing a variable at the same time
(avg <- mean(c(1:10)))

# computing correlation cofficient for non-missing observations
cor(x, y, use="pairwise.complete.obs")

# log transformed plots
plot(data$x, data$y, log="xy")

# boxplots with variable widths
boxplot(crim ~ rad, data=Boston, varwidth=TRUE)

# plotting multiple scatterplots in the same plot
matplot(data$response, data[, c("explanatory_1", "explanatory_2", ...."explanatory_n")], xlab="", ylab="")

# cross validation in R
model <- train(
  y ~ x, dataset,
  method = "lm",
  trControl = trainControl(
    method = "cv", number = 10,
    verboseIter = TRUE
  )
)

# plotting ROC curves
library(caTools)
colAUC(predicted_probabilities, actual_classes, plotROC = TRUE)
