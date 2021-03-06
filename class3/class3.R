library(dplyr)
library(purrr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(GGally)

# read the dataset
youth = read_csv("youth.csv")

# lets see what it constains
glimpse(youth)

# another view
View(youth)


# plot features in pairs to explore their relationship
# and put everything in a matrix (visually speaking)
# to see the whole dataset together
p <- ggpairs(data=youth[, c(1,2,3,4)], title="Youth survey data",
             mapping=ggplot2::aes(colour = Gender))
p


# lets bin the data and plot a bar chart
# this is a dscrete version of the distribution
summary(youth$`Height (inches)`)
NUM_BINS <- 30

youth$height_binned <- cut(youth$`Height (inches)`, breaks = NUM_BINS)
View(youth)



youth %>%
  ggplot(aes(x = height_binned)) +
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_bw()

# plot a histogram of the height field
youth %>%
  ggplot(aes(x=`Height (inches)`)) +
  geom_histogram(aes(fill=I("steelblue"), col=I("black"))) +
  theme_bw()

# now a density plot
# this is a continous version of the distribution
youth %>%
  ggplot(aes(`Height (inches)`)) +
  geom_density(alpha = 0.1, fill = I("steelblue")) +
  theme_bw()

height <- youth$`Height (inches)`
count <- length(height)

population_mean <- mean(height)
sample_mean <- mean(height[sample(count, 100)])
sample_mean
# 67.05399
N <- 10000
sample_means <- unlist(map(1:N, function(i)   mean(height[sample(count, 100)]) ))
mean(sample_means)
plot(density(sample_means))
abline(v=population_mean, col='blue')

table(youth$`How would you describe your weight?`)
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
#fit <- glm(as.factor(gender) ~ age+height+weight, data = df, family=binomial())
fit <- glm(as.factor(gender) ~ age+height+weight, data = df, family=binomial())

fit
summary(fit) # display results
confint(fit) # 95% CI for the coefficients

probs <- predict(fit, type="response") # predicted values
gender_predicted <- ifelse(probs > 0.5, "Male", "Female")

sum(gender_predicted == df$gender)/count
accuracy = mean(gender_predicted == df$gender)
accuracy
table(gender_predicted, df$gender)

table(df$gender)/count


# train test split
# Split test/training sets
set.seed(100)
library(rsample)
train_test_split <- initial_split(df, prop = 0.8)
train_test_split

# Retrieve train and test sets
train_tbl <- training(train_test_split)
test_tbl  <- testing(train_test_split)

fit <- glm(as.factor(gender) ~ age+height+weight, data = train_tbl, family=binomial())
fit
summary(fit) # display results
confint(fit) # 95% CI for the coefficients

probs <- predict(fit, type="response") # predicted values
gender_predicted <- ifelse(probs > 0.5, "Male", "Female")
accuracy = mean(gender_predicted == train_tbl$gender)
accuracy
table(gender_predicted, train_tbl$gender)

probs <- predict(fit, test_tbl, type="response") # predicted values
gender_predicted <- ifelse(probs > 0.5, "Male", "Female")
accuracy = mean(gender_predicted == test_tbl$gender)
accuracy
table(gender_predicted, test_tbl$gender)


plot(x=df$height, y=df$weight, col=ifelse(df$gender=="Male", "blue", "green"))
library(scatterplot3d)

scatterplot3d(x=df$height, y=df$weight, z=df$age, color=ifelse(df$gender=="Male", "blue", "green"))
scatterplot3d(x=df$height, y=df$weight, z=df$age, color=ifelse(df$gender=="Male", "blue", "green"))

# randomforest
library(randomForest)
youth.rf=randomForest(as.factor(gender) ~ height+weight , data = train_tbl,
                      importance=TRUE,
                      ntree=2000)
youth.rf

plot(youth.rf)

varImpPlot(youth.rf)

prediction <- predict(youth.rf, test_tbl)
mean(prediction == test_tbl$gender)


# add another feature
df = youth %>%
  rename(gender = Gender, age=Age, height=`Height (inches)`, weight=`Weight (lbs)`,
         describe_wt = `How would you describe your weight?`) %>%
  select(gender, age, height, weight, describe_wt)
View(df)

nrow(df)
df <- df %>%
  filter(!is.na(describe_wt)) %>%
  mutate(describe_wt = as.numeric(as.factor(describe_wt)))
nrow(df)

train_test_split <- initial_split(df, prop = 0.8)
train_test_split

# Retrieve train and test sets
train_tbl <- training(train_test_split)
test_tbl  <- testing(train_test_split)

youth.rf=randomForest(as.factor(gender) ~ . , data = train_tbl,
                      importance=TRUE,
                      ntree=2000)
youth.rf

plot(youth.rf)

varImpPlot(youth.rf)

prediction <- predict(youth.rf, test_tbl)
mean(prediction == test_tbl$gender)

