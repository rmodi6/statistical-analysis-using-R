# Load dataset
dataset1 <- read.csv("~/Windows/Documents/CSE518/statistical-analysis/dataset1.csv", row.names=1, quote="")
# Print summary of dataset
summary(dataset1)

# Perform ANOVA analysis
anova_results <- aov(time ~ menu, data=dataset1)
print("Summary of Anova Analysis:")
# Print summary of ANOVA
summary(anova_results)
# Get the p_value of the test
p_value <- summary(anova_results)[[1]]$Pr[[1]]
print(p_value)

if (p_value < 0.05) {  # If p_value < 0.05: reject the null hypothesis
  print("p_value is less than 0.05. Hence the results are not by chance variation and the null hypothesis is rejected. There is significant effect of independent variable menu type on dependent variable access time.")
} else {  # Else accept the null hypothesis
  print("p_value is greater than or equal to 0.05. Hence the results could be by chance variation and we cannot reject the null hypothesis. There is no significant effect of independent variable menu type on dependent variable access time.")
}


# Perform pairwise t-test     
pairwise.t.test(dataset1$time, dataset1$menu)
print("As per the results of the pairwise-t-test, only controlmenu-toolpalette pair has a p_value (0.034) < 0.05. All other pairs have a p_value >= 0.05. This means that there is statistically significant difference in access times between controlmenu and toolpalette but no significant difference in access times between other pairs of menus. To find out which is better between controlmenu and toolpalette, we can have a look at their means:")
  
# Calculate means within groups
aggregate(dataset1[, "time"], list(dataset1$menu), mean)
print("The mean access time for controlmenu (2.388) is significantly less than the mean access time of toolpalette (2.775). Thus, controlmenu is better than toolpalette in terms of access time.")

# One tailed t-test between controlmenu and toolpalette
controlmenu <- subset(dataset1, menu == "controlmenu")
toolpalette <- subset(dataset1, menu == "toolpalette")
t_test_result <- t.test(controlmenu$time, toolpalette$time, alternative = "less")
p_value <- t_test_result$p.value
print(p_value)
if (p_value < 0.05) {
  print("Since p_value < 0.05 we can reject the null hypothesis of the one tailed t-test and conclude that controlmenu has better access times than toolpalette.")
} else {
  print("Since p_value < 0.05 we can reject the null hypothesis of the one tailed t-test and conclude that controlmenu has better access times than toolpalette.")
}
 
# Visualizations 
boxplot(time~menu,
        data=dataset1,
        main="Different boxplots for each menu",
        xlab="Menu",
        ylab="Time",
        frame = FALSE,
        col = c("#DDEDAA", "#F4AC45", "#FFCAB1", "#92BFB1"),
        border="black"
)

# population distribution plots
score <- dataset1[, "time"]
group <- dataset1[, "menu"]
means <- aggregate(dataset1[, "time"], list(dataset1$menu), mean)[, 2]
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
B <- anova(lm(time ~ menu, data = dataset1))$"Mean Sq"
names(B) <- c("between", "within")
barplot(B, main = "Between vs. within", xlab = "Group (Menu)", ylab = "Sum of Squares (Time)")

# f distribution plots
means <- aggregate(dataset1[, "time"], list(dataset1$menu), mean)[, 2]
cntr <- means - mean(means)
lambda <- n*sum(cntr^2)
f <- function(x) df(x, df1 = 2, df2 = 4*(n - 1))
g <- function(x) df(x, df1 = 2, df2 = 4*(n - 1), ncp = lambda)
uplim <- qf(0.975, df1 = 2, df2 = 4*(n - 1), ncp = lambda)
curve(f, from = 0, to = uplim, lwd = 3, main = "F distributions plot",
      xlab = "F-ratio", ylab = "density")
curve(g, from = 0, to = uplim, lwd = 3, col = "red", add = TRUE)
fstat <- summary(lm(time ~ menu, data = dataset1))$fstatistic[1]
abline(v = fstat, lty = 2, col = "green", lwd = 3)
par(mfrow = c(1,1))

# Homogeneity of variances
plot(anova_results, 1)

# Normality plot
plot(anova_results, 2)
