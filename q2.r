# Load dataset
dataset2 <- read.csv("dataset2.csv", quote="")
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

# Perform pairwise t-test with dependent variable time
t_test_results <- pairwise.t.test(dataset2$time, dataset2$menu, p.adjust.method = "bonferroni", paired = TRUE)
print(t_test_results)

# One tailed t-test between toolpalette and controlmenu
toolpalette <- subset(dataset2, menu == "toolpalette")
controlmenu <- subset(dataset2, menu == "controlmenu")
t_test_result <- t.test(toolpalette$time, controlmenu$time, alternative = "greater", paired = TRUE)
p_value <- t_test_result$p.value
print(paste("p_value toolpalette-controlmenu: ", p_value))

# One tailed t-test between toolpalette and flowmenu
toolpalette <- subset(dataset2, menu == "toolpalette")
flowmenu <- subset(dataset2, menu == "flowmenu")
t_test_result <- t.test(toolpalette$time, flowmenu$time, alternative = "greater", paired = TRUE)
p_value <- t_test_result$p.value
print(paste("p_value toolpalette-flowmenu: ", p_value))

# One tailed t-test between toolpalette and toolglass
toolpalette <- subset(dataset2, menu == "toolpalette")
toolglass <- subset(dataset2, menu == "toolglass")
t_test_result <- t.test(toolpalette$time, toolglass$time, alternative = "greater", paired = TRUE)
p_value <- t_test_result$p.value
print(paste("p_value toolpalette-toolglass: ", p_value))


# Perform pairwise t-test with dependent variable error
t_test_results <- pairwise.t.test(dataset2$error, dataset2$menu, p.adjust.method = "bonferroni", paired = TRUE)
print(t_test_results)

# One tailed t-test between toolglass and controlmenu
toolglass <- subset(dataset2, menu == "toolglass")
controlmenu <- subset(dataset2, menu == "controlmenu")
t_test_result <- t.test(toolglass$error, controlmenu$error, alternative = "less", paired = TRUE)
p_value <- t_test_result$p.value
print(paste("p_value toolglass-controlmenu: ", p_value))

# One tailed t-test between toolglass and flowmenu
toolglass <- subset(dataset2, menu == "toolglass")
flowmenu <- subset(dataset2, menu == "flowmenu")
t_test_result <- t.test(toolglass$error, flowmenu$error, alternative = "less", paired = TRUE)
p_value <- t_test_result$p.value
print(paste("p_value toolglass-flowmenu: ", p_value))

# One tailed t-test between toolglass and toolpalette
toolglass <- subset(dataset2, menu == "toolglass")
toolpalette <- subset(dataset2, menu == "toolpalette")
t_test_result <- t.test(toolglass$error, toolpalette$error, alternative = "less", paired = TRUE)
p_value <- t_test_result$p.value
print(paste("p_value toolglass-toolpalette: ", p_value))


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

# between-within plot for time
B <- anova(lm(time ~ menu, data = dataset2))$"Mean Sq"
names(B) <- c("between", "within")
barplot(B, main = "Between vs. within", xlab = "Group (Menu)", ylab = "Sum of Squares (Time)", col = c("#DDEDAA", "#F4AC45"))

# between-within plot for error
B <- anova(lm(error ~ menu, data = dataset2))$"Mean Sq"
names(B) <- c("between", "within")
barplot(B, main = "Between vs. within", xlab = "Group (Menu)", ylab = "Sum of Squares (Error)", col = c("#DDEDAA", "#F4AC45"))

