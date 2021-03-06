## basic data types

# logical
would_it_rain_today <- FALSE
would_it_rain_today

# integer
number_of_students_in_this_class <- 4
number_of_students_in_this_class

# numeric
km_in_a_mile <- 1.6
km_in_a_mile

# character
name <- "my name"
name

## Data structures

# vector
vec_of_numbers <- c(3,7,10)
vec_of_logicals <- c(F,T,T)

# matrix
matrix_example = matrix(c(3,4,5,6),2,2)

# data frame
df = data.frame(col1 = c('a', 'b'), col2 = c(1,2))

# list
l = list(names = c("person1", "person2"), age = c(12, 34))

## factors

# nominal
days = factor(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
days[2]
# ordinal
state = ordered(c("disabled", "enabled"))
str(state)
# test the order of ordinal values
state[2] > state[1]


## Loops
days = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
for(day in days) {
  print(day)
}

# better to express this as
map(days, print)
# simplify to get a vector
x = unlist(map(days, print))
# now we can say
x[1]

# little taste of functional programming
library(purrr)
map(c(1,2,3), function(x) x+1) %>%
  reduce(sum)


# conditinoal statements
x = c(1,2,3,4,5)
ifelse(x > 2, T, F) # iterate over the vector x and return T (for TRUE) if an element is greater than 2, return F (for FALSE) otherwise

x[ifelse(x > 2, T, F)]


## functions
get_half_normal_mean <- function(n=10) {
  x = rnorm(n)
  mean(x[x>0])
}
get_half_normal_mean()

get_half_normal_mean(100)

