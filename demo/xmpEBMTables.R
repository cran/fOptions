

# ------------------------------------------------------------------------------
# Table 1: Moment Matched Asian Option Prices
    

    # Take the same parameter settings as used in Zhang's Table
    Zhang = ZhangTable()
    options(digits = 4)
    Zhang
    ###
    
    # Calculate entries for Table 1:
    CallRST = BoundsOnAsianOption(table = Zhang, method = "RST")$price
    CallLN  = MomentMatchedAsianOption(table = Zhang, method = "LN")$price 
    CallRG  = MomentMatchedAsianOption(table = Zhang, method = "RG")$price
    CallJI  = MomentMatchedAsianOption(table = Zhang, method = "JI")$price
    CallT   = BoundsOnAsianOption(table = Zhang, method = "T")$price            
    CallZ   = ZhangTable()[,6]
    ###
    
    # Print Call Option Values: 
    Table.1 = data.frame(cbind(Zhang[, 1:5], 
        CallRST, CallLN, CallRG, CallJI, CallT, CallZ))
    Table.1
    ###
    
    # Save Table.1 and beautify Spreadsheet file:
    # write.table(Table.1, "TableApprox.csv", sep=";")
    ###
    
    # Compute Inside Bounds:
    inside = c(
        LN = sum((CallRST < CallLN)+ (CallLN < CallT) == 2),
        RG = sum((CallRST < CallRG)+ (CallRG < CallT) == 2),
        JI = sum((CallRST < CallJI)+ (CallJI < CallT) == 2))
    inside
    ###

    
# ------------------------------------------------------------------------------
# Table 2: Gram Charlier Moment Matched Asian Option Prices
    

    # Take the same parameter settings as used in Zhang's Table
    Zhang = ZhangTable()
    options(digits = 4)
    Zhang
    ###
    
    # Calculate entries for Table 1:
    CallRST = BoundsOnAsianOption(table = Zhang, method = "RST")$price
    CallLN  = GramCharlierAsianOption(table = Zhang, method = "LN")$price 
    CallRG  = GramCharlierAsianOption(table = Zhang, method = "RG")$price
    CallJI  = GramCharlierAsianOption(table = Zhang, method = "JI")$price
    CallT   = BoundsOnAsianOption(table = Zhang, method = "T")$price            
    CallZ   = ZhangTable()[,6]
    ###
    
    # Print Call Option Values: 
    Table.2 = data.frame(cbind(Zhang[, 1:5], 
        CallRST, CallLN, CallRG, CallJI, CallT, CallZ))
    Table.2
    ###
    
    # Save Table.2 and beautify Spreadsheet file:
    # write.table(Table.2, "TableApprox.csv", sep=";")
    ###
    
    # Compute Inside Bounds:
    inside = c(
        LN = sum((CallRST < CallLN) + (CallLN < CallT) == 2),
        RG = sum((CallRST < CallRG) + (CallRG < CallT) == 2),
        JI = sum((CallRST < CallJI) + (CallJI < CallT) == 2))
    inside
    ###
    

# ------------------------------------------------------------------------------

