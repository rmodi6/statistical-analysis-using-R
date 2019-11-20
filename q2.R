# Load dataset
dataset2 <- read.csv("~/Windows/Documents/CSE518/statistical-analysis/dataset2.csv", quote="")
# Print summary of dataset
summary(dataset2)

# Perform MANOVA analysis
manova_results <- manova(cbind(time, error) ~ menu, data=dataset2)
print("Summary of Manova Analysis:")
# Print summary of MANOVA
summary(manova_results)
# Get the p_value of the test
p_value <- summary(manova_results)$stats[[1,6]]
print(p_value)

if (p_value < 0.05) {  # If p_value < 0.05: reject the null hypothesis
  print("p_value is less than 0.05. Hence the results are not by chance variation and the null hypothesis is rejected. There is significant effect of independent variable menu type on dependent variable access time and error.")
} else {  # Else accept the null hypothesis
  print("p_value is greater than or equal to 0.05. Hence the results could be by chance variation and we cannot reject the null hypothesis. There is no significant effect of independent variable menu type on dependent variable access time and error.")
}


# Perform ANOVA analysis
print("Perform ANOVA analysis to find out which dependent variable (time or error) does the independent variable (menu) has a significant effect on.")
anova_results <- summary.aov(manova_results)
print(anova_results)

# Analyze effect of menu on time
p_value_time <- anova_results$` Response time`$`Pr(>F)`[[1]]
print(paste("p_value time: ", p_value_time))
if (p_value_time < 0.05) {
  print("p_value of time is less than 0.05. Hence independent variable menu has significant effect on dependent variable time.")
} else {
  print("p_value of time is not less than 0.05. Hence independent variable menu does not have a significant effect on dependent variable time.")
}

# Analyze effect of menu on error
p_value_error <- anova_results$` Response error`$`Pr(>F)`[[1]]
print(paste("p_value error: ", p_value_error))
if (p_value_error < 0.05) {
  print("p_value of error is less than 0.05. Hence independent variable menu has significant effect on dependent variable error")
} else {
  print("p_value of error is not less than 0.05. Hence independent variable menu does not have a significant effect on dependent variable error")
}

# Perform pairwise t-test with dependent variable error
t_test_results <- pairwise.t.test(dataset2$error, dataset2$menu)
print(t_test_results)
controlmenu_toolglass <- t_test_results$p.value[[2]]
flowmenu_toolglass <- t_test_results$p.value[[5]]
toolglass_toolpalette <- t_test_results$p.value[[9]]
print(paste("p_value controlmenu and toolglass: ", controlmenu_toolglass))
print(paste("p_value flowmenu and toolglass: ", flowmenu_toolglass))
print(paste("p_value toolglass and toolpalette: ", toolglass_toolpalette))
print("As per the results of the pairwise-t-test, controlmenu-toolglass pair has a p_value (0.000082) < 0.05, flowmenu-toolglass pair has a p_value (0.000082) < 0.05 and toolglass-toolpalette pair has a p_value (0.032) < 0.05. All other pairs have a p_value >= 0.05. This means that there is statistically significant difference in errors between controlmenu-toolglass, flowmenu-toolglass and toolglass-toolpalette but no significant difference in errors between other pairs of menus.")

# Calculate means within groups
aggregate(dataset2[, "error"], list(dataset2$menu), mean)
print("The mean of error variable for toolglass menu (4.1) is significantly less than the mean of error variable for controlmenu (19.7), flowmenu (19.7) and toolpalette menu (12.8). Thus, toolglass menu is better than other menus in terms of error. We can confirm this by performing t-test of other menus with toolglass menu.")

# One tailed t-test between toolglass and controlmenu
toolglass <- subset(dataset2, menu == "toolglass")
controlmenu <- subset(dataset2, menu == "controlmenu")
t_test_result <- t.test(toolglass$error, controlmenu$error, alternative = "less")
p_value <- t_test_result$p.value
if (p_value < 0.05) {
  print("Since p_value is less than 0.05 we can reject the null hypothesis of the one tailed t-test and conclude that toolglass has less error than controlmenu.")
} else {
  print("Since p_value is not less than 0.05 we cannot reject the null hypothesis of the one tailed t-test and conclude that controlmenu has less error than toolglass.")
}

# One tailed t-test between toolglass and flowmenu
toolglass <- subset(dataset2, menu == "toolglass")
flowmenu <- subset(dataset2, menu == "flowmenu")
t_test_result <- t.test(toolglass$error, flowmenu$error, alternative = "less")
p_value <- t_test_result$p.value
if (p_value < 0.05) {
  print("Since p_value is less than 0.05 we can reject the null hypothesis of the one tailed t-test and conclude that toolglass has less error than flowmenu")
} else {
  print("Since p_value is not less than 0.05 we cannot reject the null hypothesis of the one tailed t-test and conclude that flowmenu has less error than toolglass.")
}

# One tailed t-test between toolglass and toolpalette
toolglass <- subset(dataset2, menu == "toolglass")
toolpalette <- subset(dataset2, menu == "toolpalette")
t_test_result <- t.test(toolglass$error, toolpalette$error, alternative = "less")
p_value <- t_test_result$p.value
if (p_value < 0.05) {
  print("Since p_value is less than 0.05 we can reject the null hypothesis of the one tailed t-test and conclude that toolglass has less error than toolpalette")
} else {
  print("Since p_value is not less than 0.05 we cannot reject the null hypothesis of the one tailed t-test and conclude that toolpalette has less error than toolglass.")
}

print("From the independent variable menu, toolglass menu has the most significant effect on the dependent variable error.")

# Visualizations 
boxplot(time~menu,
        data=dataset2,
        main="Different boxplots for each menu",
        xlab="Menu",
        ylab="Time",
        frame = FALSE,
        col = c("#DDEDAA", "#F4AC45", "#FFCAB1", "#92BFB1"),
        border="black"
)

boxplot(error~menu,
        data=dataset2,
        main="Different boxplots for each menu",
        xlab="Menu",
        ylab="Error",
        frame = FALSE,
        col = c("#DDEDAA", "#F4AC45", "#FFCAB1", "#92BFB1"),
        border="black"
)

# population distribution plots
score <- dataset2[, "error"]
group <- dataset2[, "menu"]
means <- aggregate(dataset2[, "error"], list(dataset2$menu), mean)[, 2]
xbot <- min(means) - 3
xtop <- max(means) + 3
plot(1, main = "Population distributions", xlim = c(xbot, xtop), ylim = c(0,0.45),
     type="n", xlab="time", ylab="density")
f <- function(x){dnorm(x, mean = means[1])}
curve(f, from = means[1] - 3, to = means[1] + 3, add = TRUE, lwd = 2, col = "red")
f <- function(x){dnorm(x, mean = means[2])}
curve(f, from = means[2] - 3, to = means[2] + 3, add = TRUE, lwd = 2, col = "green")
f <- function(x){dnorm(x, mean = means[3])}
curve(f, from = means[3] - 3, to = means[3] + 3, add = TRUE, lwd = 2, col = "blue")
f <- function(x){dnorm(x, mean = means[4])}
curve(f, from = means[4] - 3, to = means[4] + 3, add = TRUE, lwd = 2, col = "yellow")
rug(score[group == "controlmenu"], col = "red", lwd = 2, ticksize = 0.07, quiet = TRUE)
rug(score[group == "flowmenu"], col = "green", lwd = 2, ticksize = 0.07, quiet = TRUE)
rug(score[group == "toolglass"], col = "blue", lwd = 2, ticksize = 0.07, quiet = TRUE)
rug(score[group == "toolpalette"], col = "yellow", lwd = 2, ticksize = 0.07, quiet = TRUE)

# between-within plot
B <- anova(lm(error ~ menu, data = dataset2))$"Mean Sq"
names(B) <- c("between", "within")
barplot(B, main = "Between vs. within", xlab = "Group (Menu)", ylab = "Sum of Squares (Error)")
