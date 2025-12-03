# Import data
library(readr)
df <- read_csv("adult income1.csv")

# Clean and filter dataset to only include 'Male' and 'Female'
df2 <- subset(df, sex == "Male" | sex == "Female")
df2 <- subset(df, native.country == "United-States" )

# Create a new binary column for income bracket
df2$income_bracket <- ifelse(df2$income == "<=50K", "<=50K", ">50K")

# Build a contingency table: Gender vs Income bracket
 usa<- table(df2$sex[df2$native.country=="United-States"], df2$income_bracket[df2$native.country=="United-States"] )

# Chi-squared test for difference in proportions
chisq.test(usa)

# Convert counts to percentages by column: proportion inside each income group
percentages <- prop.table(usa, margin=2) * 100

# Plot: Stacked bar chart of gender proportions inside each income group
barplot(percentages, col = c("orange", "black"), xlab = "Income Bracket", ylab = "Percentage",
        main = "Proportion of Income Brackets by Gender", ylim = c(0, 100),
        legend.text = rownames(usa), args.legend = list(x = "topright"),sub = "Within UnitedStates")

# Optional: Also view proportions within each gender (transpose for barplot)
tpercentages <- prop.table(t(usa), margin=2) * 100
barplot(tpercentages, col = c("red", "green"), xlab = "Gender", ylab = "Percentage",
        main = "Income Category Within Genders", ylim = c(0, 100),
        legend.text = colnames(usa), args.legend = list(x = "topright"),sub = "Within UnitedStates")


