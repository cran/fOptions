
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
# FUNCTION:                     DESCRIPTION:
#  Currency Translated Options:
#   FEInDomesticFXOption          Foreign Exchang In Domestic Currency
#   QuantoOption                  Quanto Option
#   EquityLinkedFXOption          EquityLinked FX Option
#   TakeoverFXOption              Takeover FX Option
################################################################################


FEInDomesticFXOption = 
function(TypeFlag = c("c", "p"), S, E, X, Time, r, q, sigmaS, sigmaE, rho)
{   # A function implemented by Diethelm Wuertz           
    
    # Description:
    #   Foreign equity option struck in domestic currency
    
    # References:
    #   Haug, Chapter 2.13.1
    
    # FUNCTION:
    
    # Compute Settings:
    TypeFlag = TypeFlag[1]
    sigma = sqrt(sigmaE^2 + sigmaS^2 + 2*rho*sigmaE*sigmaS)
    d1 = (log(E*S/X) + (r-q+sigma^2/2) * Time) / (sigma*sqrt(Time))
    d2 = d1 - sigma * sqrt(Time)
    
    # Calculate Call and Put:
    if (TypeFlag == "c") {
        ForeignEquityInDomesticFX = 
            E * S * exp(-q*Time)*CND(d1) - X * exp(-r*Time)*CND(d2) }
    if (TypeFlag == "p") {
        ForeignEquityInDomesticFX = 
            X * exp(-r*Time)*CND(-d2) - E * S * exp(-q*Time)*CND(-d1) }
    
    # Return Value:
    option = list(
        price = ForeignEquityInDomesticFX, 
        call = match.call() )
    class(option) = "option"
    option
}


# ------------------------------------------------------------------------------


QuantoOption = 
function(TypeFlag = c("c", "p"), S, Ep, X, Time, r, rf, q, sigmaS, 
sigmaE, rho)
{   # A function implemented by Diethelm Wuertz           

    # Description:
    #   Fixed exchange rate foreign equity options

    # References:
    #   Haug, Chapter 2.13.2

    # FUNCTION:
    
    # Compute Settings:
    TypeFlag = TypeFlag[1]
    d1 = (log(S/X) + (rf-q-rho*sigmaS*sigmaE + sigmaS^2/2) * Time) / 
        (sigmaS*sqrt(Time))
    d2 = d1 - sigmaS*sqrt (Time)
    
    # Calculate Call and Put:
    if (TypeFlag == "c") {
        Quanto = Ep*(S*exp((rf-r-q-rho*sigmaS*sigmaE)*Time) *
            CND(d1) - X*exp(-r*Time)*CND(d2)) }
    if (TypeFlag == "p") {
        Quanto = Ep*(X*exp(-r*Time)*CND(-d2) - 
            S*exp((rf-r-q-rho*sigmaS*sigmaE)* Time)*CND(-d1)) }
    
    # Return Value:
    option = list(
        price = Quanto, 
        call = match.call() )
    class(option) = "option"
    option
}


# ------------------------------------------------------------------------------


EquityLinkedFXOption = 
function(TypeFlag = c("c", "p"), E, S, X, Time, r, rf, q, sigmaS, 
sigmaE, rho)
{   # A function implemented by Diethelm Wuertz           
    
    # Description:
    #   Equity Linked FX Option -
    
    # References:
    #   Haug, Chapter 2.13.3
    
    # FUNCTION:
    
    # Compute Settings:
    TypeFlag = TypeFlag[1]
    vS = sigmaS
    vE = sigmaE
    d1 = (log(E / X) + (r - rf + rho * vS * vE + vE ^ 2 / 2) * Time) / 
        (vE * sqrt(Time))
    d2 = d1 - vE * sqrt(Time)
    
    # Calculate Call and Put:
    if (TypeFlag == "c") {
        EquityLinkedFXO = E * S * exp(-q * Time) * CND(d1) - 
            X * S * exp((rf - r - q - rho * vS * vE) * Time) * CND(d2) }
    if (TypeFlag == "p") {
        EquityLinkedFXO = X * S * exp((rf - r - q - rho * vS * vE) * Time) * 
            CND(-d2) - E * S * exp(-q * Time) * CND(-d1) }
    
    # Return Value:
    option = list(
        price = EquityLinkedFXO, 
        call = match.call() )
    class(option) = "option"
    option
}


# ------------------------------------------------------------------------------


TakeoverFXOption = 
function(V, B, E, X, Time, r, rf, sigmaV, sigmaE, rho)
{   # A function implemented by Diethelm Wuertz           

    # Description:
    #   Takeover FX  Option -
 
    # References:
    #   Haug, Chapter 2.13.4
    
    # FUNCTION:
    
    # Compute Settings:
    v = V
    b = B
    vV = sigmaV
    vE = sigmaE
    a1 = (log(v / b) + (rf - rho * vE * vV - vV ^ 2 / 2) * Time) / 
        (vV * sqrt(Time))
    a2 = (log(E / X) + (r - rf - vE ^ 2 / 2) * Time) / 
        (vE * sqrt(Time))
    
    # Calculate:
    TakeoverFX = b * (E * exp(-rf * Time) * 
        CBND(a2 + vE * sqrt(Time), -a1 - rho * vE * sqrt(Time), -rho) - 
            X * exp(-r * Time) * CBND(-a1, a2, -rho))           
    
    # Return Value:
    option = list(
        price = TakeoverFX, 
        call = match.call() )
    class(option) = "option"
    option
}


# ******************************************************************************

