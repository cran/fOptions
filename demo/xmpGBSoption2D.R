
#
# Example:
#   Plot the scaled Price of a Generalized Black Scholes Call
#
# Description:
#   Use the function GBSoption()} to plot the scaled call
#   price c/X
#   1) as a function of S/X, the scaled asset price (note
#          doubling the asset price and strike price yields the
#      same option values), use as parameter sigma^2 T for
#      a low and a high interest rate r,
#   2) as a function of $\sigma^2 T$, the volatility and
#      Time to maturity (note doubling the volatility and
#      decreasing the time to maturity by a factor of four 
#      yields the same option values), use as parameter S/X 
#      for a low and a high interest rate $r$.:
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------


# BS Call Prices as a Function of S/X:  

    par(mfrow = c(2, 2), cex = 0.7)
    # Figure 1:
    S = seq(from = 0.4, to = 1.2, by = 0.05)
    plot(S, S, ylim = c(0, 0.5), type = "n", 
         main = "r=0.01", xlab = "S/X", ylab = "c/X")
    for (Time in seq(from = 1/12, to = 2, by = 1/12)) {
        c = GBSOption("c", S = S, X = 1, Time = Time, r = 0.01, 
            b = 0.01, sigma = 0.4)$price
        col = "steelblue3"; if (Time == 1) col = "red"
        lines (S, c, col = col) }       
    # Figure 2:
    S = seq(from = 0.4, to = 1.2, by = 0.05)
    plot(S, S, ylim = c(0, 0.5), type = "n", 
        main = "r=0.10", xlab = "S/X", ylab = "c/X")
    for (Time in seq(from = 1/12, to = 2, by = 1/12)) {
        c = GBSOption("c", S = S, X = 1, Time = Time, r = 0.10, 
            b = 0.10, sigma = 0.4)$price
        col = "steelblue3"; if (Time == 1) col = "red"
        lines (S, c, col = col) }
                    
        
# BS Call Prices as a Function of Time to Maturity: 

    # Figure 3:
    Time = seq(from = 1/12, to = 2, by = 1/12)
    plot(Time, Time, ylim = c(0, 0.5), type = "n", 
        main = "r=0.01", xlab = "Time", ylab = "c/X")
    for (S in seq(from = 0.4, to = 1.2, by = 0.05)) {
        c = GBSOption("c", S = S, X = 1, Time = Time, r = 0.01, 
            b = 0.01, sigma = 0.4)$price
        col = "steelblue3"; if (S == 1) col = "red"
        lines (Time, c, col = col) }        
    # Figure 4:
    Time = seq(from = 1/12, to = 2, by = 1/12)
    plot(Time, Time, ylim = c(0, 0.5), type = "n", 
        main = "r=0.10", xlab = "Time", ylab = "c/X")
    for (S in seq(from=0.4, to=1.2, by=0.05)) {
        c = GBSOption("c", S = S, X = 1, Time = Time, r = 0.10, 
            b = 0.10, sigma = 0.4)$price
        col = "steelblue3"; if (S == 1) col = "red" 
        lines (Time, c, col = col) }

