
#
# fOptions Functions Addon:
#
#   1 Distributions and related functions which are useful in the theory 
#     of exponential Brownian motion.
#       log-Normal, Gamma, reciprocal-Gamma, Johnson Type I Distributions
#       Moments and Derivatives
#   2 Special mathematical functions which are useful in the theory of  
#     exponential Brownian motion.
#       Part I  Error, Gamma and Related Functions 
#       Part II Confluent Hypergeometric and Related Functions.
#
# Author:
#	Diethelm Wuertz, GPL
#


################################################################################
# 1 Distributions and related functions unseful in EBM


#
# Example:
#   Adds some distributions and related functions which are
#   useful in the theory of exponential Brownian motion.
#
# Description:
#   The functions compute densities and probabilities for the 
#   log-Normal distribution, the Gamma distribution, the 
#   Reciprocal-Gamma distribution, and the Johnson Type-I
#   distribution. Functions are made available for the compution
#   of moments including the Normal, the log-Normal, the
#   Reciprocal-Gamma, and the Asian-Option Density. In addition
#   a function is given to compute numerically first and second
#   derivatives of a given function.
#
# Author:
#   (C) 2003, Diethelm Wuertz, GPL
#


################################################################################
# FUNCTION:           DENSITIES:
#  dlognorm            log-Normal density an derivatives
#  plognorm            log-Normal, synonyme for plnorm
#  dgam                Gamma density, synonyme for dgamma
#  pgam                Gamma probability, synonyme for pgamma
#  drgam               Reciprocal-Gamma density
#  prgam               Reciprocal-Gamma probability
#  djohnson            Johnson Type I density
#  pjohnson            Johnson Type I probability
# FUNCTION :          MOMENTS:
#  mnorm               Moments of Normal density
#  mlognorm            Moments of log-Normal density
#  mrgam               Moments of reciprocal-Gamma density
#  masian              Moments of Asian Option density
# FUNCTION:           NUMERICAL DERIVATIVES:
#  derivative          First and second numerical derivative
################################################################################


# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General
# Public License along with this library; if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA  02111-1307  USA

# Copyright 2003, Diethelm Wuertz


# ------------------------------------------------------------------------------


dlognorm = 
function(x, meanlog = 0, sdlog = 1, deriv = 0)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Calculates the log-Normal density or its first or
    #   second derivative. 
    
    # Details:
    #   Uses the function dlnorm().
    
    # See also:
    #   dlnorm(x, meanlog = 0, sdlog = 1, log = FALSE)
    #   plnorm(q, meanlog = 0, sdlog = 1, lower.tail = TRUE, log.p = FALSE)
    #   qlnorm(p, meanlog = 0, sdlog = 1, lower.tail = TRUE, log.p = FALSE)
    #   rlnorm(n, meanlog = 0, sdlog = 1)
    
    # FUNCTION:
    
    # Settings:
    log = FALSE
    
    # Function:
    result = dlnorm(x, meanlog, sdlog, log) 
    
    # First derivative, if desired:
    if (deriv == 1) {
        h1 = -(1/x + (log(x)-meanlog)/(sdlog^2*x))
        result = result * h1 }
        
    # Second derivative, if desired:    
    if (deriv == 2) {
        h1 = -(1/x + (log(x)-meanlog)/(sdlog^2*x))
        h2 = -(-1/x^2 + (-1/x^2)*(log(x)-meanlog)/sdlog^2 + 1/(sdlog^2*x^2))
        result = result * (h1^2 + h2) }
        
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


plognorm = 
function(q, meanlog = 0, sdlog = 1)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Calculates the log-Normal probability. 
    
    # Details:
    #   Uses the function plnorm().
    
    # See also:
    #   dlnorm(x, meanlog = 0, sdlog = 1, log = FALSE)
    #   plnorm(q, meanlog = 0, sdlog = 1, lower.tail = TRUE, log.p = FALSE)
    #   qlnorm(p, meanlog = 0, sdlog = 1, lower.tail = TRUE, log.p = FALSE)
    #   rlnorm(n, meanlog = 0, sdlog = 1)
    
    # FUNCTION:
    
    # Settings:
    log = FALSE
    
    # Function:
    result = plnorm(q, meanlog, sdlog, log) 

    # Return Value:
    result
}
    

# ------------------------------------------------------------------------------


dgam = 
function(x, alpha, beta, log = FALSE)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Calculates the Gamma density.

    # Details:
    #   The function is a synonym to "dgamma".
    
    # See also:
    #   dgamma(x, shape, rate=1, scale=1/rate, log = FALSE)
    #   pgamma(q, shape, rate=1, scale=1/rate, lower.tail = TRUE, log.p = FALSE)
    #   qgamma(p, shape, rate=1, scale=1/rate, lower.tail = TRUE, log.p = FALSE)
    #   rgamma(n, shape, rate=1, scale=1/rate)

    # FUNCTION:
    
    # Return Value:
    dgamma(x = x, shape = alpha, scale = beta, log = log)
}


# ------------------------------------------------------------------------------


pgam = 
function(q, alpha, beta, lower.tail=TRUE, log.p=FALSE)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Calculates the Gamma probability.

    # Details:
    #   The function is a synonym to "pgamma".
    
    # See also:
    #   dgamma(x, shape, rate=1, scale=1/rate, log = FALSE)
    #   pgamma(q, shape, rate=1, scale=1/rate, lower.tail = TRUE, log.p = FALSE)
    #   qgamma(p, shape, rate=1, scale=1/rate, lower.tail = TRUE, log.p = FALSE)
    #   rgamma(n, shape, rate=1, scale=1/rate)

    # FUNCTION:
    
    # Return Value:
    pgamma(q=q, shape=alpha, scale=beta, lower.tai=lower.tail, log.p=log.p)
}


# ------------------------------------------------------------------------------


drgam = 
function(x, alpha, beta, deriv=0)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Calculates the reciprocal-Gamma density.

    # See also:
    #   dgamma(x, shape, rate=1, scale=1/rate, log = FALSE)
    #   pgamma(q, shape, rate=1, scale=1/rate, lower.tail = TRUE, log.p = FALSE)
    #   qgamma(p, shape, rate=1, scale=1/rate, lower.tail = TRUE, log.p = FALSE)
    #   rgamma(n, shape, rate=1, scale=1/rate)

    # FUNCTION:
    
    # Function Value:
    gr = dgamma(x=1/x, shape=alpha, scale=beta, log=FALSE) / (x^2)
    result = gr
    
    # First Derivative:
    if (deriv > 0) {
        "h" = function(x, alpha, beta) { -(alpha+1)/x + 1/(beta*x^2) }
        gr1 = gr*h(x, alpha, beta)
        result = gr1 }
    
    # Second Derivative:
    if (deriv > 1) {
        "h1" = function(x, alpha, beta) { +(alpha+1)/x^2 - 2/(beta*x^3) }
        gr2 = gr*(h(x, alpha, beta)^2 + h1(x, alpha, beta))
        result = gr2 }
    
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


prgam = 
function(q, alpha, beta, lower.tail = TRUE, log.p = FALSE)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Calculates the reciprocal-Gamma probability
    
    # See also:
    #   dgamma(x, shape, rate=1, scale=1/rate, log = FALSE)
    #   pgamma(q, shape, rate=1, scale=1/rate, lower.tail = TRUE, log.p = FALSE)
    #   qgamma(p, shape, rate=1, scale=1/rate, lower.tail = TRUE, log.p = FALSE)
    #   rgamma(n, shape, rate=1, scale=1/rate)

    # FUNCTION:
    
    # Return Value:
    1 - pgamma(q=1/q, shape=alpha, scale=beta, lower.tai=lower.tail, log.p=log.p)
}


# ------------------------------------------------------------------------------


djohnson = 
function(x, a = 0, b = 1, c = 0, d = 1, deriv = 0)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Calculates the Johnson Type-I density.
    
    # FUNCTION:
    
    # Function Value:
    z = a + b * log( (x-c)/d )
    z1 = b / (x-c)
    phi = dnorm(z, mean = 0, sd = 1)
    johnson = phi * z1
    result = johnson
    
    # First Derivative:
    if (deriv > 0) {
        z2 = -b / (x-c)^2
        johnson1 = phi * ( z2 - z*z1^2 )
        result = johnson1 }
    
    # Second Derivative:
    if (deriv > 1) {
        z3 = 2 * b / (x-c)^3
        johnson2 = phi * ( - z*z1*z2 + z^2*z1^3 + z3 -z1^3 - 2*z*z1*z2 )
        result = johnson2 }
    
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


pjohnson = 
function(q, a = 0, b = 1, c = 0, d = 1)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Calculates the Johnson Type-I probability.
    
    # FUNCTION:
    
    # Type I:
    z = a + b * log( (x-c) / d )
    
    # Return Value:
    pnorm(q=z, mean=0, sd=1)
}


# ******************************************************************************


mlnorm = 
function(meanlog = 0, sdlog = 1)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Computes the moments for the Log-Normal distribution.
    
    # FUNCTION:
    
    # Raw Moments:
    n = 1:4
    M = exp ( n * meanlog + n^2 * sdlog^2/2 )
    
    # Centered Moments:
    m = M
    m[2] = M[2] - 2*M[1]*M[1] +   M[1]^2 
    m[3] = M[3] - 3*M[2]*M[1] + 3*M[1]*M[1]^2 -   M[1]^3 
    m[4] = M[4] - 4*M[3]*M[1] + 6*M[2]*M[1]^2 - 4*M[1]*M[1]^3 + M[1]^4 
    
    # Fischer Parameters - Skewness and Kurtosis:
    f = c(NA, NA)
    f[1] = m[3] / m[2]^(3/2)
    f[2] = m[4] / m[2]^2 - 3
    
    # Return Value:
    list(rawMoments = M, centralMoments = m, fisher = f)
}


# ------------------------------------------------------------------------------


mnorm = 
function(mean = 0, sd = 1)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes the moments for the Normal distribution.
    
    # FUNCTION:
    
    # Raw Moments:
    M = c( 
        mean, mean^2+sd^2, mean*(mean^2+3*sd*2), mean^4+6*mean^2*sd^2+3*sd^4 )
    
    # Centered Moments:
    m = M
    m[2] = M[2] - 2*M[1]*M[1] +   M[1]^2 
    m[3] = M[3] - 3*M[2]*M[1] + 3*M[1]*M[1]^2 -   M[1]^3 
    m[4] = M[4] - 4*M[3]*M[1] + 6*M[2]*M[1]^2 - 4*M[1]*M[1]^3 + M[1]^4 
    
    # Fischer Parameters - Skewness and Kurtosis:
    f = c(NA, NA)
    f[1] = m[3] / m[2]^(3/2)
    f[2] = m[4] / m[2]^2 - 3
    
    # Return Value:
    list(rawMoments = M, centralMoments = m, fisher = f)
}


# ------------------------------------------------------------------------------


mrgam = 
function(alpha = 1/2, beta = 1)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes the moments for the Reciprocal-Gamma distribution.
    
    # FUNCTION:
    
    # Raw Moments:
    M = rep(0, times = 4)
    M[1] =   1  / (beta*(alpha - 1))
    M[2] = M[1] / (beta*(alpha - 2))
    M[3] = M[2] / (beta*(alpha - 3))
    M[4] = M[3] / (beta*(alpha - 4))
    
    # Centered Moments:
    m = M
    m[2] = M[2] - 2*M[1]*M[1] +   M[1]^2 
    m[3] = M[3] - 3*M[2]*M[1] + 3*M[1]*M[1]^2 -   M[1]^3 
    m[4] = M[4] - 4*M[3]*M[1] + 6*M[2]*M[1]^2 - 4*M[1]*M[1]^3 + M[1]^4 
    
    # Fischer Parameters - Skewness and Kurtosis:
    f = c(NA, NA)
    f[1] = m[3] / m[2]^(3/2)
    f[2] = m[4] / m[2]^2 - 3
    
    # Return Value:
    list(rawMoments = M, centralMoments = m, fisher = f)
}


# ------------------------------------------------------------------------------


mjohnson = 
function(a, b, c, d)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes the moments for the Johnson Type-I distribution.
    
    # FUNCTION:
    
    # Raw Moments:
    M = c(NA, NA, NA, NA)
    
    # Centered Moments:
    m = M
    m[2] = M[2] - 2*M[1]*M[1] +   M[1]^2 
    m[3] = M[3] - 3*M[2]*M[1] + 3*M[1]*M[1]^2 -   M[1]^3 
    m[4] = M[4] - 4*M[3]*M[1] + 6*M[2]*M[1]^2 - 4*M[1]*M[1]^3 + M[1]^4 
    
    # Fischer Parameters - Skewness and Kurtosis:
    f = c(NA, NA)
    f[1] = m[3] / m[2]^(3/2)
    f[2] = m[4] / m[2]^2 - 3
    
    # Return Value:
    list(rawMoments=M, centralMoments=m, fisher=f)
}


# ------------------------------------------------------------------------------


masian = 
function(Time = 1, r = 0.045, sigma = 0.30)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes the moments for the Asian-Option distribution.
    
    # FUNCTION:
    
    # Raw Moments:
    M = DufresneAsianOptionMoments(M = 4, Time = Time, r = r, sigma = sigma)
    
    # Centered Moments:
    m = M
    m[2] = M[2] - 2*M[1]*M[1] +   M[1]^2 
    m[3] = M[3] - 3*M[2]*M[1] + 3*M[1]*M[1]^2 -   M[1]^3 
    m[4] = M[4] - 4*M[3]*M[1] + 6*M[2]*M[1]^2 - 4*M[1]*M[1]^3 + M[1]^4 
    
    # Fischer Parameters - Skewness and Kurtosis:
    f = c(NA, NA)
    f[1] = m[3] / m[2]^(3/2)
    f[2] = m[4] / m[2]^2 - 3
    
    # Return Value:
    list(rawMoments = M, centralMoments = m, fisher = f)
}


# ******************************************************************************


derivative = 
function(x, y, deriv = 1)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Calculates numerically the first or second derivative
    #   of the functuin y(x) by finite differences.
    
    # FUNCTION:
    
    # Stop in the case of wrong argument deriv:
    if (deriv < 1 || deriv > 2) stop("argument error")
    
    # Function to calculate the next derivative by differences:
    "calcderiv" = function(x,y) {
        list(x=x[2:length(x)]-diff(x)/2, y = diff(y)/diff(x))}
    
    # First Numerical Derivative:
    result = calcderiv(x,y)
    
    # Second Numerical Derivative, if desired:
    if (deriv == 2) result = calcderiv(result$x, result$y)
    
    # Return Value:
    list(x = result$x, y = result$y)
}


################################################################################
# 2 Special mathematical functions which are useful in EBM


#
# Example:
#   This is an example which shows how to implement special mathematical
#   functions in R. The examples include in Part I R functions to compute 
#   Error, Gamma and Related Functions and in Part II R functions to 
#   compute Confluent Hypergeometric and Related Functions.
#
# Description:
#   PART I: Compute Error, Gamma and Related Functions.
#     Several functions are already availalble to compute the Gamma and
#     related functions in R. We have added some missing functionality
#     including R functions to compute the Error Function, the Psi
#     Function, the Incomplete Gamma Function, the Gamma function for
#     complex arguments, and the Pochhommer Symbol.
#     These functions are required to valuate Asian Options based on
#     the theory of Exponential Brownian Motion.  
#   PART II: Compute Confluent Hypergeometric and Related Functions.
#     Uses the TOMS707 Algorithm by MARK NARDIN, W. F. PERGER and 
#     ATUL BHALLA Michigan Technological University, Copyright 1989.    
#     A numerical evaluator for the confluent hypergeometric function for 
#     complex arguments with large magnitudes using a direct summation of 
#     the Kummer series. The method used allows an accuracy of up to thirteen      
#     decimal places through the use of large real arrays and a single final 
#     division. LNCHF is a variable which selects how the result should be 
#     represented.  A '0' will return the value in standard exponential 
#     form.  A '1' will return the LOG of the result. IP is an integer     
#     variable that specifies how many array positions are desired (usually 
#     10 is sufficient). Setting IP=0 causes the program to estimate the 
#     number of array positions.                                                                 
#     The confluent hypergeometric function is the solution to  
#     the differential equation:                                
# 
#         zf"(z) + (a-z)f'(z) - bf(z) = 0    
#
#     These functions are required to valuate Asian Options based on
#     the theory of Exponential Brownian Motion.     
#
# Author:
#   (C) 2003, Diethelm Wuertz, GPL
#


################################################################################
# 2.1 DENSITIES EBM
# PART I: Functions to compute Error, Gamma and Related Functions.


################################################################################
# FUNCTION:        DESCRIPTION:
#  erf              Error function
#  [gamma]          Gamma function 
#  [lgamma]         LogGamma function, returns log(gamma)
#  [digamma]        First derivative of of LogGamma, dlog(gamma(x))/dx
#  [trigamma]       Second derivative of of LogGamma, dlog(gamma(x))/dx
#  [tetragamma]     Third derivative of of LogGamma, dlog(gamma(x))/dx
#  [pentagamma]     Fourth derivative of LogGamma, dlog(gamma(x))/dx
#  [beta]           Beta function
#  [lbeta]          LogBeta function, returns log(Beta)
#  Psi              Psi(x) (Digamma) function
#  igamma           P(a,x) Incomplete Gamma Function
#  cgamma           Gamma function for complex argument
#  Pochhammer       Pochhammer symbol
################################################################################


# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR Description.  See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General
# Public License along with this library; if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA  02111-1307  USA

# Copyright 2003, Diethelm Wuertz


# ------------------------------------------------------------------------------


erf = 
function(x)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes the Error function
    
    # FUNCTION:
    
    # Return Value:
    2*pnorm(sqrt(2)*x)
}


# ------------------------------------------------------------------------------


cgamma =  
function(x, log = FALSE) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes the Gamma Function for complex argument "x" 
    
    # Arguments:
    #   x   - complex or real vector
    #   log - if TRUE the logarithm of the gamma is calculated
    #         otherwise if FALSE, the gamma function itself 
    #         will be calculated.
    
    # Source:
    #   For the Fortran Routine:
    #   http://iris-lee3.ece.uiuc.edu/~jjin/routines/routines.html
    
    # FUNCTION:
    
    # Test for complex arguments:
    if(!is.complex(x)) x = complex(real=x, imag=0*x)  
    
    # Calculate Gamma:
    KF = 1
    if (log) KF = KF - 1
    result = rep(NA, times=length(x))
    for ( i in 1:length(x) ) {  
        value = .Fortran("cgama",
            as.double(Re(x[i])),
            as.double(Im(x[i])),
            as.integer(KF),
            as.double(0),
            as.double(0))
        result[i] = complex(real=value[[4]], imag=value[[5]]) }
    
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


Psi = 
function(x)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes the Psi or Digamma function for complex or real argument
    
    # Details:
    #   [AS} formula 6.3.1
    #   $ \Psi(x) = d ln \Gamma(z) / dz = \Gamma prime (z) / \Gamma(z) $
    
    # Arguments:
    #   x - complex or real vector
    
    # Source:
    #   For the Fortran Routine:
    #   http://iris-lee3.ece.uiuc.edu/~jjin/routines/routines.html
    
    # FUNCTION:
    
    # Psi:
    result = rep(NA, times=length(x))
    if(!is.complex(x)) {
        # Use R's digamma() function:
        result = digamma(x) }
    else {
        for ( i in 1:length(x) ) {  
            value = .Fortran("cpsi",
                as.double(Re(x[i])),
                as.double(Im(x[i])),
                as.double(0),
                as.double(0))
            result[i] = complex(real=value[[3]], imag=value[[4]]) } }
    
    # Return Value:
    result(x)
}


# ------------------------------------------------------------------------------


igamma = 
function(x, a)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes the Incomplete Gamma Function P(a, x) with 
    #   Re(a) > 0 for complex or real argument "x" and for 
    #   complex or real index "z" 
    
    # Details:
    #   [AS] formula 6.5.1  
    #   $ frac{1}{Gamma(a)}  * \int_0^x e^{-t} t^{a-1} dt $
    
    # Arguments:
    #   x   - complex or real vector
    #   a   - complex or real numeric value
    
    # FUNCTION:
    
    # igamma:
    if(!is.complex(x) && !is.complex(a)) {
        # Use R's pgamma() function:
        result = pgamma(x, a) }
    else {
        # Why not derive the result from KummersM ?
        log = FALSE
        if (log) {
            # Not yet supported:
            result = kummerM(a, a+1, -z, lnchf=1) + a*log(z) - log(a) }
        else {
            result = kummerM(a, a+1, -z, lnchf=0) * z^a / a } }
        
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


Pochhammer =  
function(x, n)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Computes Pochhammer's Symbol
    
    # Details:
    #   as defined in [AS] by formula 6.1.22
    
    # FUNCTION:
    
    # $ (z)_0 = 1 $
    # $ (z)_n = z(z+1)(z+2) \dots (z+n-1) = frac{\Gamma(z+n)}{Gamma(z)} $
    
    # In case of wrong argument Type:
    Pochhammer = NA
    
    # For Complex Arguments:
    if(is.complex(x)) {
        Pochhammer = cgamma(x+n)/cgamma(x) }
    
    # For real Arguments:
    if(is.real(x)) {
        Pochhammer = gamma(x+n)/gamma(x) }
        
    # Return Value:
    Pochhammer
}

    
################################################################################
# 2.2 DENSITIES EBM
# PART II: Compute Confluent Hypergeometric and Related Functions. 

 
################################################################################
# FUNCTION:       DESCRIPTION:               
#   kummerM        Computes Confluent Hypergeometric Function of the 1st Kind
#   kummerU        Computes Confluent Hypergeometric Function of the 2nd Kind
#   whittakerM     Computes Whittaker's M Function
#   whittakerW     Computes Whittaker's M Function
#   hermiteH       Computes the Hermite Polynomial
################################################################################


# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR Description.  See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General
# Public License along with this library; if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA  02111-1307  USA

# Copyright 2003, Diethelm Wuertz


# ------------------------------------------------------------------------------


kummerM =  
function(x, a, b, lnchf = 0, ip = 0) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Calculate the Confluent Hypergeometric Function of the First  
    #   Kind for complex argument "x" and complex indexes "a" and "b"
    
    # FUNCTION:
    
    # Test for complex arguments:
    if(!is.complex(x)) x = complex(real = x, imag = 0*x)
    if(!is.complex(a)) a = complex(real = a, imag = 0)
    if(!is.complex(b)) b = complex(real = b, imag = 0)    
    
    # Calculate KummerM:
    chm = rep(complex(real = 0, imag = 0), length = length(x))
    value = .Fortran("chfm",
        as.double(Re(x)),
        as.double(Im(x)),
        as.double(Re(a)),
        as.double(Im(a)),
        as.double(Re(b)),
        as.double(Im(b)),
        as.double(Re(chm)),
        as.double(Im(chm)),
        as.integer(length(x)),
        as.integer(lnchf),
        as.integer(ip))
    result = complex(real = value[[7]], imag = value[[8]]) 
    
    # Return Value:
    result
}


# ------------------------------------------------------------------------------


kummerU =  
function(x, a, b, ip = 0) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Calculate the Confluent Hypergeometric Function of the Second 
    #   Kind for complex argument "x" and complex indexes "a" and "b"
  
    # FUNCTION:
    
    # Todo ...
    lnchf = 0
    
    # Test for complex arguments:
    if(!is.complex(x)) x = complex(real = x, imag = 0*x)
    if(!is.complex(a)) a = complex(real = a, imag = 0)
    if(!is.complex(b)) b = complex(real = b, imag = 0)
    
    # Calculate KummerU:
    # From KummerM:
    # Uses the formula ...
    #   pi/sin(pi*b) [ M(a,b,z) / (Gamma(1+a-b)*Gamma(b)) - 
    #        x^(1-b) * M(1+a-b,2-b,z) / (Gamma(a)*Gamma(2-b)) ]
    ans = ( pi/sin(pi*b) ) * (
        kummerM(x, a = a, b = b, lnchf = lnchf, ip=ip) /
            ( cgamma(1+a-b)*cgamma(b) ) - (x^(1-b)) * 
        kummerM(x, a = (1+a-b), b=2-b, lnchf = lnchf, ip = ip) / 
            ( cgamma(a)*cgamma(2-b) ) )

    # Return Value: 
    ans
}


# ------------------------------------------------------------------------------


whittakerM = 
function(x, kappa, mu, ip = 0) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes Whittaker's M Function
    
    # FUNCTION:
    
    # Test for complex arguments:
    if(!is.complex(x)) x = complex(real=x, imag=0*x)
    if(!is.complex(kappa)) kappa = complex(real=kappa, imag=0)
    if(!is.complex(mu)) mu = complex(real=mu, imag=0)
    
    # Calculate:
    ans = exp(-x/2) * x^(1/2+mu) * kummerM(x, 1/2+mu-kappa, 1+2*mu, ip = ip)
        
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


whittakerW = 
function(x, kappa, mu, ip = 0) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes Whittaker's M Function
    
    # FUNCTION:
    
    # Test for complex arguments:
    if(!is.complex(x)) x = complex(real = x, imag = 0*x)
    if(!is.complex(kappa)) kappa = complex(real = kappa, imag = 0)
    if(!is.complex(mu)) mu = complex(real = mu, imag = 0)
    
    # Calculate:
    ans = exp(-x/2) * x^(1/2+mu) * kummerU(x, 1/2+mu-kappa, 1+2*mu, ip = ip)
        
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


hermiteH = 
function(x, n, ip = 0)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Computes the Hermite Polynomial 
    
    # FUNCTION:
    
    # Result:
    ans = 2^n * Re ( kummerU(x^2, -n/2, 1/2, ip = ip) )
    
    # Return Value:
    ans
    
}


################################################################################

