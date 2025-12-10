# Import data
library(readr)
df <- read_csv("adult income1.csv")

# Clean and filter dataset to only include 'Male' and 'Female'
df2 <- subset(df, sex == "Male" | sex == "Female")

# Create a new binary column for income bracket
df2$income_bracket <- ifelse(df2$income == "<=50K", "<=50K", ">50K")

# Build a contingency table: Gender vs Income bracket
pt <- table(df2$sex, df2$income_bracket)

# Chi-squared test for difference in proportions
chisq.test(pt)

# Convert counts to percentages by column: proportion inside each income group
percentages <- prop.table(pt, margin=2) * 100

# Plot: Stacked bar chart of gender proportions inside each income group
barplot(percentages, col = c("blue", "pink"), xlab = "Income Bracket", ylab = "Percentage",
        main = "Proportion of Income Brackets by Gender", ylim = c(0, 100),
        legend.text = rownames(pt), args.legend = list(x = "topright"))

# Optional: Also view proportions within each gender (transpose for barplot)
tpercentages <- prop.table(t(pt), margin=2) * 100
barplot(tpercentages, col = c("red", "green"), xlab = "Gender", ylab = "Percentage",
        main = "Income Category Within Genders", ylim = c(0, 100),
        legend.text = colnames(pt), args.legend = list(x = "topright"))

