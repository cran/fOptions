
#
# Example: 
#	Price American Options Using the BS Approximation
#	
# Description:
#  	The Bjerksund and Stensland approximation an be used to
# 	price American Options on stocks, futures and currencies. 
#	Consider a call option with nine months to expiry. The
#	stock price is 42, the strike price is 40, the risk free 
#	rate is 4% p.a., the dividend yield is 8% p.a., the volatility 
#	is 35% p.a..
#
# Reference:
#  	Haug [1997], Chapter 1.4.3, page 26
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------


# Price American Options using the BS Approximation:

	BSAmericanApproxOption("c", S = 42, X = 40, Time = 0.75, r = 0.04, 
		b = 0.04-0.08, sigma = 0.35)

# Compare with a similar European Call:

	GBSOption("c", S = 42, X = 40, Time = 0.75, r = 0.04, 
		b = 0.04-0.08, sigma = 0.35)
	
			