library(dplyr)
library(purrr)
library(ggplot2)

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
  ggplot(aes(x = factor(height_binned)))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal()

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

hist(youth$`Height (inches)`, breaks=30)
plot(density(youth$`Height (inches)`))
