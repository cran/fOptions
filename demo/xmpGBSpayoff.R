
#
# Example: 
#   Plot Profit / Loss versus Asset Price Graphs
#
# Description:
#   Plot graphs displaying the profit/loss versus the
#   asset price for the following four situations:
#   Buying a call, writing a call, buying a put, and
#   writing a put.
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------


# Buying a Call:

    par(mfrow = c(3, 2))
    plot(x = c(50, 150), y = c(0, 0), xlim = c(50, 150), ylim = c(-16, 16), 
        type = "l", main = "Buying a Call", xlab = "Asset Price", 
        ylab = " Loss / Profit")
    lines(x = c(50, 100), y = c(-8, -8))
    lines(x = c(100, 130), y = c(-8, 14))

# Writing a Call:

    plot(x = c(50, 150), y = c(0, 0), xlim = c(50, 150), ylim = c(-16, 16), 
        type = "l", main = "Writing a Call", xlab = "Asset Price", 
        ylab = " Loss / Profit")
    lines(x = c(50, 100), y = c(8, 8))
    lines(x = c(100, 130), y = c(8, -14))

# Buying a Put:

    plot(x = c(50, 150), y = c(0, 0), xlim = c(50, 150), ylim = c(-16, 16), 
        type = "l", main = "Buying a Put", xlab = "Asset Price", 
        ylab = " Loss / Profit")
    lines(x = c(100, 150), y = c(-8, -8))
    lines(x = c(70, 100), y = c(14, -8))

# Writing a Put:

    plot(x = c(50, 150), y = c(0, 0), xlim = c(50, 150), ylim = c(-16, 16), 
        type = "l", main = "Writing a Put", xlab = "Asset Price", 
        ylab = " Loss / Profit")
    lines(x = c(100, 150), y = c(8, 8))
    lines(x = c(70, 100), y = c(-14, 8))

