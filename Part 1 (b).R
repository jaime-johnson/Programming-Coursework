#Importing Libraries
library(ggplot2)

#Defining the probability density function f(x)
f<- function(x){
  return(0.5 * exp(-abs(x)))
}

#Given that J and N are fixed
J <- 4  
N <- 2000  

#Creating a multidimensional array to store the chains
x <- array(0, dim = c(J, N + 1))
x[1, 1] <- 0
x[1, 2] <- 1
x[1, 3] <- 2
x[1, 4] <- 3

#Generating 100 evenly spaced s values in between 0.001 and 1
s_values <- seq(from = 0.001, to = 1, length.out = 100)

#Defining a function to carry out step 2 of the Random walk Metropolis Algorithm
step_2 <- function(s, J, N, x, f) {
  for (j in 1:J) {
    for (n in 2:(N + 1)) {
      x_star <- rnorm(1, mean = x[j, n - 1], sd = s)
      r <- f(x_star) / f(x[j, n - 1])
      u <- runif(1)
      
      if (log(u) < log(r)) {
        x[j, n] <- x_star
      } else {
        x[j, n] <- x[j, n - 1]
      }
    }
  }
  
  return(x[, 2:(N + 1)]) #dropping the x_0 values so that array x contains only generated samples
}

#Defining a function to calulate R_hat
calculate_R_hat <- function(x) {
  M_j <- apply(x, 1, mean)
  V_j <- apply(x, 1, var)
  W <- mean(V_j)
  M <- mean(M_j)
  B <- mean((M_j - M)^2)
  
  R_hat <- sqrt((B + W) / W)
  
  return(R_hat)
}

#Calculating R_hat for each s value
R_hat_for_s <- vector("numeric", length(s_values))
for (i in seq_along(s_values)) {
  s <- s_values[i]
  generated_x <- step_2(s, J, N, x, f)  # Assuming x and f are already defined
  R_hat <- calculate_R_hat(generated_x)
  R_hat_for_s[i] <- R_hat
}

#Plotting the R_hat values for each s
data <- data.frame(s_values = s_values, R_hat_for_s = R_hat_for_s)
ggplot(data, aes(x = s_values, y = R_hat_for_s)) +
  geom_line(color='darkblue') +
  labs(x = "s values", y = "R_hat values") +
  theme_minimal()


#Calculating R_hat when s=0.001
x_sample <- array(0, dim = c(J, N + 1))
x[1, 1] <- 0
x[1, 2] <- 1
x[1, 3] <- 2
x[1, 4] <- 3
generated_x_sample <- step_2(0.001, J, N, x_sample, f)
R_hat_sample <- calculate_R_hat(generated_x_sample)
print(paste('R hat value when s is 0.001 =', R_hat_sample))