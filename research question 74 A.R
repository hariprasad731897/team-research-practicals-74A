#Load the dataset
df<- read_csv("~/UH/R works/adult income1.csv")

# Subset data (optional – example: remove any cases with missing or extreme values)
df2 <- subset(df, age >= 16 & age <= 62 & education.num >= 1 & education.num <= 16)

# Check normality for education_num(for each gender)
hist(df2$education.num[df2$sex == "Male"], main = "Histogram of Education (Males)",breaks = 10,xlab = "education",ylab = "No of People")
hist(df2$education.num[df2$sex == "Female"], main = "Histogram of Education (Females)",breaks = 10,xlab = "education",ylab = "No of People")

# Check normality for age (for each gender)
hist(df2$age[df2$sex == "Male"], main = "Histogram of Age (Males)",breaks = 5,xlab = "age",ylab = "No of People")
hist(df2$age[df2$sex == "Female"], main = "Histogram of Age (Females)",breaks = 5,xlab = "age",ylab = "No of People")

# Correlation test for Males
cor.test(df2$age[df2$sex == "Male"], df2$education.num[df2$sex == "Male"], method = "pearson")
# Correlation test for Females
cor.test(df2$age[df2$sex == "Female"], df2$education.num[df2$sex == "Female"], method = "pearson")

# Scatterplot for Male
plot(df2$age[df2$sex == "Male"], df2$education.num[df2$sex == "Male"], xlab = "Age", ylab = "Education-num", main = "Age vs Education (Males)")
abline(lm(education.num ~ age, data=df2[df2$sex == "Male",]), col="blue")

# Scatterplot for female
plot(df2$age[df2$sex == "Female"], df2$education.num[df2$sex == "Female"], xlab = "Age", ylab = "Education-num", main = "Age vs Education (Females)")
abline(lm(education.num ~ age, data=df2[df2$sex == "Female",]), col="red")
