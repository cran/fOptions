
#
# Example: 
#	Compute Halton low discrepancy numbers
#
# Description: 
#	This example calculates quasi random numbers created from
#	Halton's low discrepancy sequence. Plot the sequence, its
#	autocorrelation function and histogram. Compare the results
#	in low and high dimensions.
#
# Functions: 
#	runif.halton(), rnorm.halton()
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
	runif.halton(n = 20, dimension = 5, init = TRUE)
	# Part 2a: Re-Initialize and Calculate 10 Sequences in 5 dimensions:
	runif.halton(n = 10, dimension = 5, init = TRUE)
	# Part 2b: Calculate next 10 sequences in 5 dimensions
	runif.halton(n = 10, dimension = 5, init = FALSE)
	# Extract the direction numbers from the seed:
	runif.halton.seed

# Create uniform distributed numbers:

	x = runif.halton(n = 500, dimension = 25)
	plot(x[,1], x[,2], main = "Uniform Halton: 1 - 2")
	plot(x[,24], x[,25], main = "Uniform Halton: 24 - 25")
	# Autocorrelation:
	acf(x[ ,1], main = "Uniform Halton: d=1")
	acf(x[ ,25], main = "Uniform Halton: d=25")
	# Histogram:
	hist(x[ ,1])
	hist(x[ ,25])

# Create normal distributed numbers:

	x = rnorm.halton(n = 500, dimension = 25)
	plot(x[,1], x[,2], main = "Normal Halton: 1 - 2")
	plot(x[,24], x[,25], main = "Normal Halton: 24 - 25")
	# Autocorrelation:
	acf(x[ ,1], main = "Normal Halton: d=1")
	acf(x[ ,25], main = "Normal Halton: d=25")
	# Histogram:
	hist(x[ ,1])
	hist(x[ ,25])
	par(ask = FALSE)

