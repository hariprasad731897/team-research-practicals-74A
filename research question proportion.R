# Import data
library(readr)
df <- read_csv("adult income1.csv")

# Clean and filter dataset to only include 'Male' and 'Female'
df2 <- subset(df, sex == "Male" | sex == "Female")
df2 <- subset(df, native.country == "United-States" | native.country == "Mexico"|native.country == "India")

# Create a new binary column for income bracket
df2$income_bracket <- ifelse(df2$income == "<=50K", "<=50K", ">50K")

# Build a contingency table: Gender vs Income bracket
 usa<- table(df2$sex[df2$native.country=="United-States"], df2$income_bracket[df2$native.country=="United-States"] )

# Chi-squared test for difference in proportions
chisq.test(usa)

# Convert counts to percentages by column: proportion inside each income group
percentages <- prop.table(usa, margin=2) * 100

# Plot: Stacked bar chart of gender proportions inside each income group
barplot(percentages, col = c("blue", "pink"), xlab = "Income Bracket", ylab = "Percentage",
        main = "Proportion of Income Brackets by Gender", ylim = c(0, 100),
        legend.text = rownames(usa), args.legend = list(x = "topright"),sub = "Within UnitedStates")

# Optional: Also view proportions within each gender (transpose for barplot)
tpercentages <- prop.table(t(usa), margin=2) * 100
barplot(tpercentages, col = c("red", "green"), xlab = "Gender", ylab = "Percentage",
        main = "Income Category Within Genders", ylim = c(0, 100),
        legend.text = colnames(usa), args.legend = list(x = "topright"),sub = "Within UnitedStates")

#Based on MEXICO
# Build a contingency table: Gender vs Income bracket
mex<- table(df2$sex[df2$native.country=="Mexico"], df2$income_bracket[df2$native.country=="Mexico"] )

# Chi-squared test for difference in proportions
chisq.test(mex)

# Convert counts to percentages by column: proportion inside each income group
percentages <- prop.table(mex, margin=2) * 100

# Plot: Stacked bar chart of gender proportions inside each income group
barplot(percentages, col = c("orange", "gray"), xlab = "Income Bracket", ylab = "Percentage",
        main = "Proportion of Income Brackets by Gender", ylim = c(0, 100),
        legend.text = rownames(mex), args.legend = list(x = "topright"),sub = "Within Mexico")

# Optional: Also view proportions within each gender (transpose for barplot)
tpercentages <- prop.table(t(mex), margin=2) * 100
barplot(tpercentages, col = c("blue", "yellow"), xlab = "Gender", ylab = "Percentage",
        main = "Income Category Within Genders", ylim = c(0, 100),
        legend.text = colnames(mex), args.legend = list(x = "topright"),sub = "Within Mexico")

#Based on India

# Build a contingency table: Gender vs Income bracket
ind<- table(df2$sex[df2$native.country=="India"], df2$income_bracket[df2$native.country=="India"] )

# Chi-squared test for difference in proportions
chisq.test(ind)

# Convert counts to percentages by column: proportion inside each income group
percentages <- prop.table(usa, margin=2) * 100

# Plot: Stacked bar chart of gender proportions inside each income group
barplot(percentages, col = c("black", "white"), xlab = "Income Bracket", ylab = "Percentage",
        main = "Proportion of Income Brackets by Gender", ylim = c(0, 100),
        legend.text = rownames(ind), args.legend = list(x = "topright"),sub = "within India")

# Optional: Also view proportions within each gender (transpose for barplot)
tpercentages <- prop.table(t(ind), margin=2) * 100
barplot(tpercentages, col = c("violet", "green"), xlab = "Gender", ylab = "Percentage",
        main = "Income Category Within Genders", ylim = c(0, 100),
        legend.text = colnames(ind), args.legend = list(x = "topright"),sub = "Within India")

