
#
# Example:	
#	Compute special mathematical functions used in the theory
#	of exponential Brownian Motion.
#
# Description:
#	Part I: Calculate Error, Gamma and Related Functions
#   Part II: Calculate Confluent Hypergeometric and Related Functions
#
# Author:
#	(C) 2003, Diethelm Wuertz, GPL
#


################################################################################
## Part I: Calculate Error, Gamma and Related Functions


## Examples 1:
## Abramowitz-Stegun: Figure 6.1
   x = seq(-4.01, 4.01, by = 0.011)
   plot(x, gamma(x), ylim = c(-5,5), type = "l", main = "Gamma Function")
   lines(x = c(-4, 4), y = c(0, 0))
   ###

   
## Examples 2:   
## Abramowitz-Stegun: Figure 6.1
   x = seq(-4.01, 4.01, by=0.011)
   plot(x, Psi(x), ylim=c(-5, 5), type = "l", main = "Psi Function")
   lines(x = c(-4, 4), y = c(0, 0))
   # Note: Is digamma defined for positive values only ???

 
## Examples 3:
## Abramowitz-Stegun: Figure 6.3.
## gammaStar
   gammaStar = function(x, a) { GammaInc(x,a)/x^a }
   # ... create Figure, to be done !!!
   ###

   
## Examples 4:
## Abramowitz-Stegun: Formula 6.5.12
## Relation to Confluent Hypergeometric Functions
   a = sqrt(2)
   x = pi
   Re ( (x^a/a) * kummerM(-x, a, 1+a) )
   Re ( (x^a*exp(-x)/a) * kummerM(x, 1, 1+a) )
   pgamma(x,a)*gamma(a)
   gammaInc(x, a)
   ###
   
 
## Examples 5:
## Abramowitz-Stegun: Tables 6.7
   x = 1
   y = seq(0, 5, by = 0.1); x = rep(x, length = length(y))
   z = complex(real = x, imag = y)
   c = cgamma(z, log = TRUE)
   cbind(y, Re(c), Im(c))
   ###
   
   
## Examples 6:
## Examples [AS] 4-8:
   options(digits=10)
   gamma(6.38); lgamma(56.38)							 # 1/2
   Psi(6.38); Psi(56.38)								 # 3/4
   cgamma (complex(real = 1, imag = -1), log = TRUE ) 	 # 5
   cgamma (complex(real = 1/2, imag = 1/2), log = TRUE ) # 6
   cgamma (complex(real = 3, imag = 7), log = TRUE ) 	 # 7/8 
   ###
   
 
################################################################################
## Part II: Calculate Confluent Hypergeometric and Related Functions


## Example 1:
## Abramowitz-Stegun: Formula 13.6.3/13.6.21
   x <- c(0.001, 0.01, 0.1, 1, 10, 100, 1000)  
   nu <- 1; a <- nu+1/2; b <- 2*nu+1
   M <- Re ( kummerM(x = 2*x, a = a, b = b) )
   Bessel <- gamma(1+nu)*exp(x)*(x/2)^(-nu)*besselI(x, nu)
   cbind(x, M, Bessel)
   ###
   

## Example 2:
## Abramowitz-Stegun: Formula 13.6.14
   x <- c(0.001, 0.01, 0.1, 1, 10, 100, 1000)  
   M <- Re ( kummerM(2*x, a = 1, b = 2) )
   Sinh <- exp(x)*sinh(x)/(x)
   cbind(x, M, Sinh)
   # Now the same for complex x:
   y <- rep(1, length = length(x))
   x <- complex(real = x, imag = y)
   M <- kummerM(2*x, a = 1, b = 2)
   Sinh <- exp(x)*sinh(x)/(x)
   cbind(x, M, Sinh)
   ###


## Example 3:
## Abramowitz-Stegun: Formula 13.1.3
   x <- c(0.001, 0.01, 0.1, 1, 10, 100, 1000) 
   a <- 1/3; b <- 2/3
   U <- Re ( kummerU(x, a = a, b = b) )
   cbind(x, U)
   ###
   
   
## Example 4: 
## Abramowitz-Stegun: Example 13.
## Whittaker:
   AS <- c(1.10622, 0.57469)
   W <- c(
     whittakerM(x = 1, kappa = 0, mu = -0.4),
     whittakerW(x = 1, kappa = 0, mu = -0.4) )
   data.frame(AS, W)
   ###
  

## Example 5:
## Abramowitz-Stegun: Example 17.
   x <- seq(0, 16, length = 200)
   plot(x = x, y = kummerM(x, -4.5, 1), type = "l", ylim = c(-25,125),
     main = "Figure 13.2:  M(-4.5, 1, x)")
   lines(x = c(0, 16), y = c(0,0), col = "red")
   ###


################################################################################

