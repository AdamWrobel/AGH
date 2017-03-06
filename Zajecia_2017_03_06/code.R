# Numerical derivative
num.deriv <- function(f, x, h = 1e-05)
{
  return((f(x + h) - f(x - h))/(2*h))
}

print(num.deriv(sqrt, 4, .1))
print(num.deriv(sqrt, 4, .01))

# reproducing the random numbers with a given seed
loss.dist <- function(seed, N)
{
  set.seed(seed)
  return(runif(N))
}

print(loss.dist(seed=1, N=4))
print(loss.dist(seed=1, N=4))  # same seed - same sequence
print(loss.dist(seed=10, N=4)) # different seed - different sequency

# check if matrix is positive semidefinite
R <- matrix(c(1,.5,.5,1), nrow = 2)
print(min(eigen(R)$value))


# generating correlated numbers
R <- matrix(c(1,.5,.5,1), nrow = 2)
EG <- eigen(R)
mx <- EG$vectors %*% diag(sqrt(EG$values))
V <- matrix(rnorm(1000), nrow = 2)
print(cor(t(mx%*%V)))

# probability functions
# names for distributions in R: norm, pois, unif, gamma etc
# p + name - cumulative distribution function
# d + name - probability distribution function (density)
# q + name - quantile function (inverse cdf)
# r + name - random numbers generating function
# Examples for Gamma distributions:
# pgamma
# dgamma
# qgamma
# rgamma
