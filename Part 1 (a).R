#Importing Libraries
library(ggplot2)

#Defining the probability density function f(x)
f<- function(x){
           return(0.5 * exp(-abs(x)))
}

#Given that
N<-1000
s<-1

#Creating a vector for x with all zeros initially
#The length of the vector is N+1 because it has to store values from x_0 to x_N
x <- vector(mode="numeric",length=N+1)

#Step 1
x[1] <- 1

#Checking the length of the vector x
length(x)

#Step 2
for (i in (1 : N+1)){
  #stimulating a random number x_star from normal distribution with mean x_i-1 and standard deviation s
  x_star = rnorm(1,mean = x[i-1], sd=s)
  #computing the ratio
  r <- f(x_star) / f(x[i-1])
  #generating a random number u from uniform distribution between 0 and 1 
  u <- runif(1,0,1)
  if (log(u) < log(r)) {
    x[i] <- x_star
  } else {
    x[i] <- x[i-1]
  }
}

#Dropping the initial value from the vactor so that the array x contains only the generated samples x_1 to x_N
x <- x[-1]

#Checking the length of x
length(x)

#Converting the vector x to data frame
df = data.frame(x)

ggplot(data = df, aes(x = x)) +
    #Histogram
    geom_histogram(aes(y = ..density..), color = 'darkgreen', fill = 'lightgreen') +
    #Kernel Density plot
    geom_density(color = 'black', bw = 0.4, size = 1.2, alpha = 1.5) +
    #Graph of f(x)
    geom_function(fun = f, color = 'orange', size = 1.2)+
    labs(x = "Generated Samples (x_1,...,x_N)", y = "Density")+
    theme_bw()


#Calculating the mean of the generated sample
mean_value <- mean(x)
cat("Monte Carlo estimate of the mean is", mean_value)

#Calculating the standard deviation of the generated sample 
sd_value <- sd(x)
cat("Monte Carlo estimate of the standard deviation is", sd_value)