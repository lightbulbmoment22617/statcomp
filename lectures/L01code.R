#### Basic variable/object assignment and functions ####

# comments are marked with # at the start of the comment

x <- 3 # Assign the value 3 to a variable named 'x'
x = 3 # Also allowed but usually discouraged
# 3 <- x ## What's this?? Error.

x # will (usually) print the value
print(x) # will (nearly always) print the value

#### Vectors, indexing, and vectorised functions ####

y <- c(1, 2, 3, 7, 5) # "combine" several values into a vector
y
z <- 1:100 # All the integers from 1 to 100
z

z + 20 ## Compute and print
cos(z) ## Vectoriesd functions; compute element by element

A <- matrix(1:6, 2, 3)
A

z[26] # Extract element nunmber 26 from z
cos(z[26]) # Extract element nunmber 26 from z, than compute cosine
cos(z)[26] # Compute the cosine of all elements of z, then extract element nunmber 26

#### 'list': collections of objects ####

mycollection <- c(x, y, z, A) # Combine objects?
mycollection
str(mycollection) # Show the internal storage structure

mycoll <- list(x, y, z, A)
mycoll
str(mycoll)
mycoll[[4]]

mycoll2 <- list(x = x, y = y, my_matrix = A)
mycoll2
mycoll2$my_matrix
mycoll2[[3]]
mycoll2[["my_matrix"]]

#### 'data.frame': Structured data columns, one observation per row ####

df <- data.frame(x = 10:20, y = rnorm(11))
df
str(df)

#### 'formula' objects for linear models ####

formula <- y ~ x
formula
str(formula)

estimate <- lm(y ~ x, df) # Estimate the model y = a + b x + e
estimate
summary(estimate) # More useful way of printing these objects

#### Data type conversion ####

as.list(df)

#### Extension libraries; installing and loading or accessing ####

# Example:

install.packages("styler") # Install an R package to your user account

library(Matrix) # Load a package into the active R session

#### Script vs structured programming ####

# Script style:

A <- matrix(rnorm(10 * 10), 10, 10)
b <- rnorm(10)
x <- solve(A + t(A), b)

# Structured style, helps reusability in more complex situations!

solve_my_problem <- function(A, b) {
  solve(A + t(A), b)
}

big_matrix <- matrix(rnorm(10 * 10), 10, 10)
some_vector <- rnorm(10)

x <- solve_my_problem(big_matrix, some_vector)
x2 <- solve_my_problem(5, 2)
x3 <- solve_my_problem(matrix(runif(400), 20,20), rnorm(20))
x4 <- solve_my_problem(A = big_matrix, b = some_vector)
