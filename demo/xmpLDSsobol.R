
#
# Example: 
#	Compute Sobol low discrepancy numbers
#
# Description: 
#	This example calculates the first 20 Sobol numbers in 5 
#	dimension. In a second step the generator is called twice, 
#	the first time with re-initialization flag set to TRUE. 
#	Print the seed and extract the direction numbers.
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------


# Settings:

	par(mfrow = c(3, 2), cex = 0.5)
	par(ask = TRUE)

# Uniform Quasi Random Numbers:

	# Part 1: Calculate 20 sequences in 5 dimensions:
	runif.sobol(n = 20, dimension = 5, scrambling = 0, init = TRUE)
	# Part 2a: Re-Initialize and Calculate 10 Sequences in 5 dimensions:
	runif.sobol(n = 10, dimension = 5, scrambling = 0, init = TRUE)
	# Part 2b: Calculate next 10 sequences in 5 dimensions
	runif.sobol(n = 10, dimension = 5, scrambling = 0, init = FALSE)
	# Extract the direction numbers from the seed:
	runif.sobol.seed

# Create uniform distributed numbers:

	x = runif.sobol(n = 500, dimension = 25, scrambling = 0)
	plot(x[,1], x[,2], main = "Uniform Sobol: 1 - 2")
	plot(x[,24], x[,25], main="Uniform Sobol: 24 - 25")
	# Autocorrelation:
	acf(x[ ,1], main = "Uniform Sobol: d=1")
	acf(x[ ,25], main = "Uniform Sobol: d=25")
	# Histogram:
	hist(x[ ,1])
	hist(x[ ,25])

# Create normal distributed numbers: 

	x = rnorm.sobol(n = 500, dimension = 25, scrambling = 1)
	plot(x[,1], x[,2], main = "Normal Sobol: 1 - 2")
	plot(x[,24], x[,25], main = "Normal Sobol: 24 - 25")
	# Autocorrelation:
	acf(x[ ,1], main = "Normal Sobol: d=1")
	acf(x[ ,25], main = "Normal Sobol: d=25")
	# Histogram:
	hist(x[ ,1])
	hist(x[ ,25])
	par(ask = FALSE)

