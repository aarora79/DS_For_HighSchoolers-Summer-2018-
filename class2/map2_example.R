library(purrr)

x <- c(1,2,3)
y <- c('a', 'b', 'c')
z = "map2 example "
z2 = " something else"
result <- map2(x, y, function(a, b, c, d) print(paste0(c, b, " = ", a, d)), z, z2)


x = data.frame(col1=c('a', 'b', 'c'), col2=c(1,2,3))
x
x[2, "col2"]
mean(x[["col2"]])
x[['col2']]
x[[1]]
x[,"col2"]
