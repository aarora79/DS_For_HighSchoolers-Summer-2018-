library(dplyr)
library(purrr)
library(ggplot2)
library(tidyr)
library(tidyverse)


# read the dataset
youth = read_csv("youth.csv")

# lets see what it constains
glimpse(youth)

# another view
View(youth)

# lets bin the data and plot a bar chart
# this is a dscrete version of the distribution
summary(youth$`Height (inches)`)
NUM_BINS <- 30

youth$height_binned <- cut(youth$`Height (inches)`, breaks = NUM_BINS)
View(youth)

youth %>%
  ggplot(aes(x = height_binned))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_bw()

# plot a histogram of the height field
youth %>%
  ggplot(aes(x=`Height (inches)`)) +
  geom_histogram(aes(fill=I("white"), col=I("black"))) +
  theme_minimal()

# now a density plot
# this is a continous version of the distribution
youth %>%
  ggplot(aes(`Height (inches)`)) +
  geom_density(alpha = 0.1, fill = I("blue")) +
  theme_minimal()

height <- youth$`Height (inches)`
count <- length(height)

population_mean <- mean(height)
# 67.05399
N <- 100000
sample_means <- unlist(map(1:N, function(x)   mean(height[sample(count, 100)]) ))
mean(sample_means)
plot(density(sample_means))


# create a glm model
df = youth %>%
  rename(gender = Gender, age=Age, height=`Height (inches)`, weight=`Weight (lbs)`) %>%
  select(gender, age, height, weight)
View(df)

library(GGally)
ggscatmat(df, columns = 2:4, color="gender", alpha=0.8) +
  ggtitle("Correlation in various elements of the youth dataset") + 
  theme(axis.text.x = element_text(angle=-40, vjust=1, hjust=0, size=10))


library(glmnet)
fit <- glm(as.factor(gender) ~ age+height+weight, data = df, family=binomial())
fit
summary(fit) # display results
confint(fit) # 95% CI for the coefficients

probs <- predict(fit, type="response") # predicted values
gender_predicted <- ifelse(probs > 0.5, "Male", "Female")
accuracy = mean(gender_predicted == df$gender)
accuracy
table(gender_predicted, df$gender)

table(df$gender)/count
