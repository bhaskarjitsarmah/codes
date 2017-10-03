# assigning and printing a variable at the same time
(avg <- mean(c(1:10)))

# computing correlation cofficient for non-missing observations
cor(x, y, use="pairwise.complete.obs")

# log transformed plots
plot(data$x, data$y, log="xy")

# boxplots with variable widths
boxplot(crim ~ rad, data=Boston, varwidth=TRUE)

# plotting multiple scatterplots in the same plot
