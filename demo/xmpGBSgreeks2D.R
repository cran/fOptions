
#
# Example: 
#   Investigate Premium and Sensitivities of a Currency Option
#
# Description:
#   As a simple illustration of how the Greeks can be used consider
#   someone holding an at-the-money currency call on USD put on DEM
#   with the following characteristics: underlying price 1.7000,
#   strike price 1.700$, time to maturity 270 days, DEM (domestic)
#   interest rate 6% p.a., USD (foreign) interest rate 3% p.a.,
#   and volatility 10% p.a.. Calculate the option value and the
#   sensitivities for these parameters and then plot the sensitivities 
#   in the range between 1.4000 and 2.0000 for times to maturity of 
#   1, 30, 90, and 270 days to get an overview about the whole range 
#   of prices of the underlying.
#   Suppose after one week the underlying price were to rise to
#   1.7500 DEM, interest rates were to fall 1%, and volatility
#   were to rise 2%. what effect would this combination have on the
#   price of the option? Analyze the separate impacts, and calculate 
#   the combined effect to the option rise. 
#
# Reference:
#   L. Galitz, "Financial Engineering" (1994)
#   Chapter 10, p. 249
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------


# Characterisitics of the BS Option:

    GBSCharacteristics("c", S = 1.7000, X = 1.7000, 
        Time = 270/365, r = 0.06, b = 0.06-0.03, sigma = 0.10)

# Plot Greeks:

    par(mfrow = c(2, 2), cex = 0.7)
    S = seq(from = 1.4000, to = 2.0000, length = 250)
    Selection = c("Delta", "Theta", "Vega", "Rho")
    PlotYmin  = c(    0.0,    -0.6,   0.0,    0.0)
    PlotYmax  = c(    1.0,     0.0,   0.6,    1.2)
    for (i in 1:4){
        plot(S, S, ylim = c(PlotYmin[i], PlotYmax[i]), type = "n", 
             xlab = "S", ylab = Selection[i], main = Selection[i])
        for (Days in c(1, 30, 90, 270)) {
            greek = GBSGreeks(Selection[i], "c", S = S, X = 1.7000, 
                Time = Days/365, r = 0.06, b = 0.06-0.03, sigma = 0.1)
            lines (S, greek) } }

# Analyzing the Separate Impacts:

    Characteristics = 
        GBSCharacteristics("c", S = 1.7000, X = 1.7000, 
        Time = 270/365, r = 0.06, b = 0.06-0.03, sigma = 0.10)
    Changes = list (
        DueToPrice = (1.7500 - 1.7000) * Characteristics$delta,
        DueToTime = 7/365 * Characteristics$theta,
        DueToVolatility = (0.12 - 0.10) * Characteristics$vega,
        DueToInterest = (0.05 - 0.06) * Characteristics$rho)
    print(Changes)
    TotalChange = Changes$DueToPrice + Changes$DueToTime + 
        Changes$DueToVolatility + Changes$DueToInterest
    print(TotalChange)
    OldPremium = Characteristics$premium
    print(OldPremium)
    NewPremium = GBSOption("c", S = 1.7500, X = 1.7000, 
        Time = 263/365, r = 0.05, b = 0.05-0.03, sigma = 0.12)$price
    print(NewPremium)
    RealTotalChange = NewPremium - OldPremium
    print(RealTotalChange)  

 