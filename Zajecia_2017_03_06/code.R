num.deriv <- function(f, x, h = 1e-05)
{
  return((f(x + h) - f(x - h))/(2*h))
}

print(num.deriv(sqrt, 4, .1))
print(num.deriv(sqrt, 4, .01))
