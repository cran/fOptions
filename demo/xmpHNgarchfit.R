
#
# Example: 
#	HN Options Pricing - Max LogLikelihood Estimation
#
# Description:
#	Simulate time series with the same parameters as those
#	fitted by Heston and nandi to the SP500 data ranging
#	from 2/8/1992 to 30/12/1994, finally re-fit the parameters.
#   DATA:
#     Index Value at 2:30 PM
#     No. of observations 755
#     r - TBill rate (3.5%)
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


# SP500 Data from MASS Package:

	require(MASS)
	data(SP500)
	returns = SP500[505:(505+755)]/100
	model.sym = list(lambda = 0.2, omega = 1.6e-6, alpha = 1e-6, 
		beta = 0.92, gamma = 0, rf = 0.035/252)
	model.asym = list(lambda = 0.7, omega = 5e-6, alpha = 1e-6,
		beta = 0.59, gamma = 421, rf = 0.035/252)

# Estimate the Symmetric HN-GARCH(1,1) Parameters: 

  	fit.sym = hngarchFit(x = returns, model = model.sym, 
  		symmetric = TRUE)
  	fit.sym
  		
  
# Estimate the Asymmetric HN-GARCH(1,1):

  	fit.asym = hngarchFit(x = returns, model = model.asym, 
  		symmetric = FALSE)
	fit.asym
	
	
# Summarize Results:	
	
	data.frame(cbind(
		SYM = unlist(model.sym), 
		SYM.FIT = unlist(fit.sym$model),
		ASYM = unlist(model.asym),
		ASYM.FIT = unlist(fit.asym$model)
		))
	
# Diagnostic Analysis:

	par(mfcol = c(3,2), cex=0.5)
	summary(fit.sym)
	summary(fit.asym)
	
# Moment Statistics:

	unlist(hngarchStats(model.sym))
	unlist(hngarchStats(model.asym))
	unlist(hngarchStats(fit.sym$model))
	unlist(hngarchStats(fit.asym$model))
	
	
