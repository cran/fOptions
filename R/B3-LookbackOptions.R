
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General 
# Public License along with this library; if not, write to the 
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
# MA  02111-1307  USA

# Copyrights (C) 
# this R-port: 
#   by Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
# for the code accessed (or partly included) from other R-ports:
#   R: see R's copyright and license file
# for Haug's Option Pricing Formulas:
#   Formulas are implemented along the book and the Excel spreadsheets of 
#     E.G. Haug, "The Complete Guide to Option Pricing"; documentation
#     is partly taken from www.derivicom.com which implements
#     a C Library based on Haug. For non-academic and commercial use 
#     we recommend the professional software from "www.derivicom.com".  


################################################################################
# FUNCTION:                           DESCRIPTION:
# Lookback Options:
#   FloatingStrikeLookbackOption        Floating Strike Lookback Option
#   FixedStrikeLookbackOption           Fixed Strike Lookback Option
#   PTFloatingStrikeLookbackOption      Partial Floating Strike LB Option
#   PTFixedStrikeLookbackOption         Partial Fixed Strike LB Option  
#   ExtremeSpreadOption                 Extreme Spread Option
################################################################################


FloatingStrikeLookbackOption = 
function(TypeFlag = c("c", "p"), S, SMinOrMax, Time, r, b, sigma)
{   # A function implemented by Diethelm Wuertz           
    
    # Description:
    #   Floating strike lookback options

    # References:
    #   Haug, Chapter 2.9.1

    # FUNCTION:
    
    # Comute Settungs:
    TypeFlag = TypeFlag[1]
    if (TypeFlag == "c") m = SMinOrMax # Min
    if (TypeFlag == "p") m = SMinOrMax # Max
    a1 = (log(S / m) + (b + sigma^2 / 2) * Time) / (sigma * sqrt(Time))
    a2 = a1 - sigma * sqrt (Time)
    
    # Calculate Call and Put:
    if (TypeFlag == "c") 
        FloatingStrikeLookback = S * exp ((b - r) * Time) * CND(a1) -
            m * exp(-r * Time) * CND(a2) + exp (-r * Time) *
            sigma^2 / (2 * b) * S * ((S / m)^(-2 * b / sigma^2) *
            CND(-a1 + 2 * b / sigma * sqrt(Time)) - exp(b * Time) * CND(-a1))
    if (TypeFlag == "p") 
        FloatingStrikeLookback = m * exp (-r * Time) * CND(-a2) -
            S * exp((b - r) * Time) * CND(-a1) + exp (-r * Time) * 
            sigma^2 / (2 * b) * S * (-(S / m)^(-2 * b / sigma^2) *
            CND(a1 - 2 * b / sigma * sqrt(Time)) + exp(b * Time) * CND(a1)) 
    
    # Return Value:
    option = list(
        price = FloatingStrikeLookback, 
        call = match.call() )
    class(option) = "option"
    option
}


# ------------------------------------------------------------------------------


FixedStrikeLookbackOption = 
function(TypeFlag = c("c", "p"), S, SMinOrMax, X, Time, r, b, sigma)
{   # A function implemented by Diethelm Wuertz           

    # Description:
    #   Fixed strike lookback options

    # References:
    #   Haug, Chapter 2.9.2

    # FUNCTION:
    
    # Compute Settings:
    TypeFlag = TypeFlag[1]
    if (TypeFlag == "c") m = SMinOrMax
    if (TypeFlag == "p") m = SMinOrMax
    d1 = (log(S / X) + (b + sigma^2 / 2) * Time) / (sigma * sqrt(Time))
    d2 = d1 - sigma * sqrt (Time)
    e1 = (log(S / m) + (b + sigma^2 / 2) * Time) / (sigma * sqrt(Time))
    e2 = e1 - sigma * sqrt (Time)
    
    # Calculate Call and Put:
    if (TypeFlag == "c" && X > m) 
            FixedStrikeLookback = S * exp ((b - r) * Time) * CND(d1) -
                X * exp(-r * Time) * CND(d2) +
                S * exp (-r * Time) * sigma^2 / (2 * b) * 
            (-(S / X)^(-2 * b / sigma^2) *
                CND(d1 - 2 * b / sigma * sqrt(Time)) + exp(b * Time) * CND(d1))
    if (TypeFlag == "c" && X <= m)  
            FixedStrikeLookback = exp (-r * Time) * (m - X) +
                S * exp((b-r) * Time) * CND(e1) - exp(-r * Time) * m * CND(e2) +
                S * exp (-r * Time) * sigma^2 / (2 * b) * 
            (-(S / m)^(-2 * b / sigma^2) *
                CND(e1 - 2 * b / sigma * sqrt(Time)) + exp(b * Time) * CND(e1))
    if (TypeFlag == "p" && X < m) 
            FixedStrikeLookback = -S * exp ((b - r) * Time) * CND(-d1) +
                X * exp(-r * Time) * CND(-d1 + sigma * sqrt(Time)) +
                S * exp (-r * Time) * sigma^2 / (2 * b) * 
            ((S / X)^(-2 * b / sigma^2) *
                CND(-d1 + 2 * b / sigma * sqrt(Time)) - exp(b*Time) * CND(-d1))
    if (TypeFlag == "p" && X >= m) 
            FixedStrikeLookback = exp (-r * Time) * (X - m) -
                S * exp((b - r) * Time) * CND(-e1) + 
            exp(-r * Time) * m * CND(-e1 + sigma * sqrt(Time)) +
                exp (-r * Time) * sigma^2 / (2 * b) * S * 
            ((S / m)^(-2 * b / sigma^2) *
                CND(-e1 + 2 * b / sigma * sqrt(Time)) - exp(b*Time) * CND(-e1))
    
    # Return Value:
    option = list(
        price = FixedStrikeLookback, 
        call = match.call() )
    class(option) = "option"
    option
}


# ------------------------------------------------------------------------------


PTFloatingStrikeLookbackOption = 
function(TypeFlag = c("c", "p"), S, SMinOrMax, time1, Time2, r, b, 
sigma, lambda)
{   # A function implemented by Diethelm Wuertz           

    # Description:
    #   Partial-time floating strike lookback options

    # References:
    #   Haug, Chapter 2.9.3
    
    # FUNCTION:
    
    # Compute Settings:
    TypeFlag = TypeFlag[1]
    T2 = Time2
    t1 = time1
    if (TypeFlag == "c") m = SMinOrMax
    if (TypeFlag == "p") m = SMinOrMax
    d1 = (log(S / m) + (b + sigma^2 / 2) * T2) / (sigma * sqrt(T2))
    d2 = d1 - sigma * sqrt (T2)
    e1 = (b + sigma^2 / 2) * (T2 - t1) / (sigma * sqrt(T2 - t1))
    e2 = e1 - sigma * sqrt (T2 - t1)
    f1 = (log(S / m) + (b + sigma^2 / 2) * t1) / (sigma * sqrt(t1))
    f2 = f1 - sigma * sqrt (t1)
    g1 = log (lambda) / (sigma * sqrt(T2))
    g2 = log (lambda) / (sigma * sqrt(T2 - t1))
        
    # Calculate Call and Puts:
    if (TypeFlag == "c") {
        part1 = S * exp ((b - r) * T2) * CND(d1 - g1) -
            lambda * m * exp(-r * T2) * CND(d2 - g1)
        part2 = exp (-r * T2) * sigma^2 /
            (2 * b) * lambda * S * ((S / m)^(-2 * b / sigma^2) *
            CBND(-f1 + 2*b*sqrt(t1) / sigma, -d1 + 2 * b * sqrt(T2) / sigma -
            g1, sqrt(t1 / T2)) - exp (b * T2) * lambda^(2 * b / sigma^2) *
            CBND(-d1 - g1, e1 + g2, -sqrt(1 - t1 / T2))) +
            S * exp ((b - r)*T2) * CBND(-d1 + g1, e1 - g2, -sqrt(1 - t1 / T2))
        part3 = exp (-r*T2) * lambda * m * CBND(-f2, d2 - g1, -sqrt(t1 / T2)) -
            exp (-b * (T2-t1)) * exp((b - r)*T2) * (1 + sigma^2 / (2 * b)) *
            lambda * S * CND(e2 - g2) * CND(-f1) }
    if (TypeFlag == "p") {
        part1 = lambda * m * exp (-r * T2) * CND(-d2 + g1) -
            S * exp((b - r) * T2) * CND(-d1 + g1)
        part2 = -exp (-r * T2) * sigma^2 /
            (2 * b) * lambda * S * ((S / m)^(-2 * b / sigma^2) *
            CBND(f1 - 2 * b * sqrt(t1) / sigma, d1 - 2 * b * sqrt(T2) / sigma +
            g1, sqrt(t1 / T2)) - exp (b * T2) * lambda^(2 * b / sigma^2) *
            CBND(d1 + g1, -e1 - g2, -sqrt(1 - t1 / T2))) -
            S * exp ((b - r)*T2) * CBND(d1 - g1, -e1 + g2, -sqrt(1 - t1 / T2))
        part3 = -exp (-r*T2) * lambda*m * CBND(f2, -d2 + g1, -sqrt(t1 / T2)) +
            exp (-b * (T2-t1)) * exp((b - r)*T2) * (1 + sigma^2 / (2 * b)) *
            lambda * S * CND(-e2 + g2) * CND(f1) }
    PartialFloatLookback = part1 + part2 + part3
    
    # Return Value:
    option = list(
        price = PartialFloatLookback, 
        call = match.call() )
    class(option) = "option"
    option
}


# ------------------------------------------------------------------------------


PTFixedStrikeLookbackOption = 
function(TypeFlag = c("c", "p"), S, X, time1, Time2, r, b, sigma) 
{   # A function implemented by Diethelm Wuertz           

    # Description:
    #   Partial Time Fixed Strike Lookback Option 

    # References:
    #   Haug, Chapter 2.9.4
    
    # FUNCTION:
    
    # Compute Settings:
    TypeFlag = TypeFlag[1]
    d1 = (log(S / X) + (b + sigma^2 / 2) * Time2) / 
        (sigma * sqrt(Time2))
    d2 = d1 - sigma * sqrt(Time2)
    e1 = ((b + sigma^2 / 2) * (Time2 - time1)) / 
        (sigma * sqrt(Time2 - time1))
    e2 = e1 - sigma * sqrt(Time2 - time1)
    f1 = (log(S / X) + (b + sigma^2 / 2) * time1) / (sigma * sqrt(time1))
    f2 = f1 - sigma * sqrt(time1)
    
    # Calculate Call and Put:
    if (TypeFlag == "c") {
        PartialFixedLB = S * exp((b - r) * Time2) * 
            CND(d1) - exp(-r * Time2) * X * 
            CND(d2) + S * exp(-r * Time2) * sigma^2 / (2 * b) * 
                (-(S / X)^(-2 * b / sigma^2) * 
            CBND(d1 - 2 * b * sqrt(Time2) / sigma, -f1 + 2 * b * sqrt(time1) / 
                sigma, -sqrt(time1 / Time2)) + exp(b * Time2) * CBND(e1, d1, 
                sqrt(1 - time1 / Time2))) - S * exp((b - r) * Time2) * 
            CBND(-e1, d1, -sqrt(1 - time1 / Time2)) - 
                X * exp(-r * Time2) * 
            CBND(f2, -d2, -sqrt(time1 / Time2)) + exp(-b * (Time2 - time1)) * 
                (1 - sigma^2 / (2 * b)) * S * exp((b - r) * Time2) * 
            CND(f1) * 
            CND(-e2) }
   if (TypeFlag == "p") {
        PartialFixedLB = X * exp(-r * Time2) * 
            CND(-d2) - S * exp((b - r) * Time2) * 
            CND(-d1) + S * exp(-r * Time2) * sigma^2 / (2 * b) * 
                ((S / X)^(-2 * b / sigma^2) * CBND(-d1 + 2 * b * 
                sqrt(Time2) / sigma, f1 - 2 * b * sqrt(time1) / sigma, 
                -sqrt(time1 / Time2)) - exp(b * Time2) * 
            CBND(-e1, -d1, sqrt(1 - time1 / Time2))) + 
                S * exp((b - r) * Time2) * 
            CBND(e1, -d1, -sqrt(1 - time1 / Time2)) + X * exp(-r * Time2) * 
            CBND(-f2, d2, -sqrt(time1 / Time2)) - exp(-b * (Time2 - time1)) * 
                (1 - sigma^2 / (2 * b)) * S * exp((b - r) * Time2) * 
            CND(-f1) * 
            CND(e2) }
    
    # Return Value:
    option = list(
        price = PartialFixedLB, 
        call = match.call() )
    class(option) = "option"
    option
}


# ------------------------------------------------------------------------------


ExtremeSpreadOption = 
function(TypeFlag = c("c", "p", "cr", "pr"), S, SMin, SMax, time1, Time2, 
r, b, sigma)
{   # A function implemented by Diethelm Wuertz           
    
    # Description:
    #   Extreme Spread Option 

    # References:
    #   Haug, Chapter 2.9.5
   
    # FUNCTION:
    
    # Compute Settings:
    TypeFlag = TypeFlag[1]
    v = sigma
    Time = Time2
    if (TypeFlag == "c"  || TypeFlag == "cr") { eta = +1 }
    if (TypeFlag == "p"  || TypeFlag == "pr") { eta = -1 }
    if (TypeFlag == "c"  || TypeFlag == "p")  { kappa = +1 }
    if (TypeFlag == "cr" || TypeFlag == "pr") { kappa = -1 }          
    if (kappa * eta == +1) { Mo = SMax }
    if (kappa * eta == -1) { Mo = SMin }    
    mu1 = b - v^2 / 2
    mu = mu1 + v^2
    m = log(Mo/S)   
    ExtremeSpread = NA
    
    # Extreme Spread Option:
    if (kappa == 1) {
    ExtremeSpread = eta * (S * exp((b - r) * Time) * (1 + v^2 / (2 * b)) * 
        CND(eta * (-m + mu * Time) / (v*sqrt(Time))) - 
            exp(-r * (Time - time1)) * S * exp((b - r) * Time) * 
            (1 + v^2 / (2 * b)) * 
        CND(eta * (-m + mu * time1) / (v*sqrt(time1))) + exp(-r * Time) * 
            Mo * CND(eta * (m - mu1 * Time) / (v*sqrt(Time))) - 
            exp(-r * Time) * Mo * v^2 / (2 * b) * 
            exp(2 * mu1 * m / v^2) * 
        CND(eta * (-m - mu1 * Time) / (v*sqrt(Time))) - exp(-r * Time) * Mo * 
        CND(eta * (m - mu1 * time1) / (v*sqrt(time1))) + exp(-r * Time) * 
            Mo * v^2 / (2 * b) * exp(2 * mu1 * m / v^2) * 
        CND(eta * (-m - mu1 * time1) / (v*sqrt(time1)))) }
    
    # Reverse Extreme Spread Option:  
    if (kappa == -1) {  
    ExtremeSpread = -eta * (S * exp((b - r) * Time) * (1 + v^2 / (2 * b)) * 
        CND(eta * (m - mu * Time) / (v*sqrt(Time))) + exp(-r * Time) * Mo * 
        CND(eta * (-m + mu1 * Time) / (v*sqrt(Time))) - exp(-r * Time) * 
            Mo * v^2 / (2 * b) * exp(2 * mu1 * m / v^2) * 
        CND(eta * (m + mu1 * Time) / (v*sqrt(Time))) - S * 
            exp((b - r) * Time) * (1 + v^2 / (2 * b)) * 
        CND(eta * (-mu * (Time - time1)) / (v*sqrt(Time - time1))) - 
            exp(-r * (Time - time1)) * S * exp((b - r) * Time) * 
            (1 - v^2 / (2 * b)) * 
        CND(eta * (mu1 * (Time - time1)) / (v*sqrt(Time - time1)))) }      
    
    # Return Value:
    option = list(
        price = ExtremeSpread, 
        call = match.call() )
    class(option) = "option"
    option              
}


# ******************************************************************************

