
#*******************************************************************************
# fOptions - A SOFTWARE COLLECTION FOR FINANCIAL ENGINEERS
# PART IV: Pricing and Hedging of Options
#
# collected by Diethelm Wuertz
#    
#*******************************************************************************
                                                        

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


#*******************************************************************************

# Default Settings:

    xmpOptions = function(prompt = "") {invisible(prompt)}
 

.First.lib = 
function(lib, pkg)
{   # A function implemented by D. Wuertz
    
    # Package:
    cat("\nfOptions:   Valuation of Options ")
    
    # Requires:
    # DEBUG <- FALSE
    # sink("@sink@")           
    # ...
    # sink()
    # unlink("@sink@")

    # Load dll: 
    library.dynam("fOptions", pkg, lib)
}

