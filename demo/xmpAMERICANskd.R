
#
# Example: 
#	Price American Stock Options with Known Dividends
#	
# Description:
#	American Calls on stocks paying a single dividend can 
#	be priced by the Roll-Geske-Whaley formula.
#	Consider an American-style call option on a stock that
#  	will pay a dividend of $4$ in exactly three months. The 
#	stock price is $80$, the strike price is $82$, time to 
#	maturity is 4 months, the risk-free interest rate is 6%, 
#	and the volatility is 30%. Note, that the result will 
#	be 4.3860, whereas the value of a similar European call 
#	would be $3.5107. What happens when no dividends are 
#	paid, D=0?
#
# Reference:
#   Haug [1997], Chapter 1.4.1, page 19
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------


# American Stock Options with known Dividends:
	RollGeskeWhaleyOption(S = 80, X = 82, time1 = 1/4, Time2 = 1/3, 
		r = 0.06, D = 4, sigma = 0.30)
	
# What happens when D=0?	
	RollGeskeWhaleyOption(S = 80, X = 82, time1 = 1/4, Time2 = 1/3, 
		r = 0.06, D = 0, sigma = 0.30)

