
rbinom(1, 1, .5)
rbinom(10, 1, .5)


d1 <- rbinom(10, 1, .5)
sum(d1)

rbinom(1, 10, .5)

rbinom(10, 10, .5)
rbinom(10, 10, .8)
rbinom(10, 10, .2)

flips <- rbinom(100000, 10, .5)
hist(flips)

flips == 5
mean(flips == 5)

pbinom(5, 10, .5)

rnorm(10000, 65, 3.5)

heights <- rnorm(10000, 65, 3.5)
hist(heights)

f <- function(x){ dnorm(x, mean = 65, sd = 3.5) }
integrate(f, 70, Inf)

pnorm(70, 65, 3.5)
1 - pnorm(70, 65, 3.5)
