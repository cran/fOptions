
#
# Example:
#	HN Options Pricing - GARCH(1,1) Simulation
#
# Description:
#	Simulate time series with the same parameters as those
#	fitted by Heston and nandi to the SP500 data ranging
#	the three years from 01/08/1992 to 12/30/1994.
#   DATA:
#     Index Value at 2:30 PM
#     No. of observations 755
#     r - TBill rate (3.7%)
#   RESULT:
#           lambda   omega   alpha  beta  gamma  THETA   PERS  MLLH
#     Sym:     0.7  1.6e-6  1.0e-6  0.92    ---   9.2%   0.92  3492
#     Asym:    0.2  5.0e-6  1.0e-6  0.59    421   8.0%   0.77  3504
#  
# Reference:
# 	S. Heston and S. Nandi, 1997
# 	A Closed-Form GARCH Option Pricing Model
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------
	
	
# Fit a Symmetric HN-GARCH(1,1) Process:

 	set.seed(4711)
 	model = list(lambda = 0.7, omega = 1.6e-6, alpha = 1e-6, 
 		beta = 0.92, gamma = 0, rf = 0.037/252)
 	ts.sym = hngarchSim(model, n = 755, n.start = 100)
 	par(mfcol = c(3, 2), cex = 0.5)
 	ts.plot(ts.sym, main = "Symmetric Data")
		
# Fit an Asymmetric HN-GARCH(1,1) Process:

 	set.seed(4711)
 	model = list(lambda = 0.2, omega = 5.0e-6, alpha = 1e-6, 
 		beta = 0.59, gamma = 421, rf = 0.037/252)
 	ts.asym = hngarchSim(model, n = 755, n.start = 100)
 	ts.plot(ts.asym, main = "Asymmetric Data")
 	# Plot Both:
 	ts.plot(ts.asym, main = "Both Data Sets")
 	lines(ts.sym, col = "red")
			
# ACF Plots:

	result = acf(abs(ts.sym), main = "ACF: Symmetric Data")
	result = acf(abs(ts.asym), main = "ACF: Asymmetric Data")
	
	