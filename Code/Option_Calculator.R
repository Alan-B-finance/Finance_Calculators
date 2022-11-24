Option <- setClass(
  
  "Option",
  
  slots = c(
    S0 = "numeric", #Underlying Price
    K = "numeric", #Strike
    r = "numeric", #Risk Free Rate during the live of the Option
    cD = "numeric", #Context Date in yyyymmdd format
    mD = "numeric", #Maturity Date in yyyymmdd format
    div = "numeric", #Dividend yield
    flag = "character", #c for call, p for put
    vol = "numeric", #yearly volatility
    p = "numeric", #Price of the option
    Years = "numeric", #time to maturity in years
    flavor = "character", #a for american, e for european, b for bermudan
    ExerciseDates = "vector", #vector of dates when an option can be exercised
    Barrier = "numeric", #Price of a barrier
    BarrierType = "character", #Type of a barrier
    
    DiscountFactor = "numeric" #Overall discount Factor to maturity
  ),
  
  prototype = list(
    div = 0
  ),
  
  validity = function(object){
    if(object@S0 < 0){
      return("Error: Underlying price can't be lower than 0")
    } else if(object@K < 0){
      return("Error: Strike can't be lower than 0")
    } else if(length(object@cD) > 0 & length(object@mD) > 0){
      if(nchar(object@cD) != 8 | nchar(object@mD) != 8){
        return("Error: Please provide dates in the yyyymmdd format")
      }
    } else if(!(object@flag %in% c("c", "p"))){
      return("Error: The flag can either be c (call) or p (put)")
    } else if(length(object@p > 0)){
        if(object@p < 0){
          return("Error: Price of an option can't be lower than 0")
      }
    } else if(length(object@Years > 0)){
      if(object@Years < 0){
        return("Error: Years to maturity can't be lower than 0")
      }
    } 
    if(!(object@flavor %in% c("a", "e", "b"))){
      return("Error: The flag can either be a (american), e (european) or b (bermudean)")
    } 
    if(object@flavor == "b"){
      if(length(object@ExerciseDates) == 0){
        return("Error: Provide exercise dates for the bermudean option")
      } else if((object@N %% (length(object@ExerciseDates)+1)) != 0){
        return("Error: The number of steps needs to be a multiple of the number of dates + 1")
      }
    }
    if(length(object@BarrierType) > 0){
      if(!(object@BarrierType %in% c("KO", "KI"))){
        return("Error: Barrier Type should be KO (knock-out) or KI (knock-in)")
      } else if(length(object@BarrierType) == 0){
        return("Error: Barrier Type provided but no barrier")
      }
    }
  },
  
  contains=c("VIRTUAL")
)

setMethod("initialize", "Option",
          function(.Object, ...) {
            .Object <- callNextMethod(.Object, ...)
            validObject(.Object)
            
            #If dates are provided calculate the years to maturity
            if(length(.Object@Years) == 0){
              .Object@Years <- as.numeric(as.Date(as.character(.Object@mD), format("%Y%m%d")) - as.Date(as.character(.Object@cD), format("%Y%m%d")))/365
            }
            .Object@DiscountFactor <- 1/exp(.Object@r - .Object@div)^(.Object@Years)
            
            return(.Object)
          }
)

TwoAssetFXOptions <- setClass(
  
  "TwoAssetFXOptions",
  
  slots = c(
    S01 = "numeric", #Underlying Price of the first asset 
    S02 = "numeric", #Underlying Price of the second asset 
    K1 = "numeric", #Strike on the first option
    K2 = "numeric", #Strike on the second option
    r1 = "numeric", #First Risk Free Rate
    r2 = "numeric", #Second Risk Free Rate
    r3 = "numeric", #Third Risk Free Rate
    cD = "numeric", #Context Date in yyyymmdd format
    mD = "numeric", #Maturity Date in yyyymmdd format
    flag1 = "character", #c for call, p for put
    flag2 = "character", #c for call, p for put
    vol1 = "numeric", #first yearly volatility
    vol2 = "numeric", #second yearly volatility
    p = "numeric", #Price of the option
    Years = "numeric", #time to maturity in years
    flavor = "character", #a for american, e for european, b for bermudan
    ExerciseDates = "vector", #vector of dates when an option can be exercised
    Barrier = "numeric", #Price of a barrier
    BarrierType = "character", #Type of a barrier
    Corr = "numeric", #Correlation between underlyings
    
    DiscountFactor = "numeric" #Overall discount Factor to maturity
  ),
  
  validity = function(object){
    
  }
)

setMethod("initialize", "TwoAssetFXOptions",
          function(.Object, ...) {
            .Object <- callNextMethod(.Object, ...)
            validObject(.Object)
            
            #If dates are provided calculate the years to maturity
            if(length(.Object@Years) == 0){
              .Object@Years <- as.numeric(as.Date(as.character(.Object@mD), format("%Y%m%d")) - as.Date(as.character(.Object@cD), format("%Y%m%d")))/365
            }
            .Object@DiscountFactor <- 1/exp(.Object@r3)^(.Object@Years)
            
            return(.Object)
          }
)

TreeOption <- setClass(
  
  "TreeOption",
  
  slots = c(
    N = "numeric", #Number of steps

    YearsPerTimeStep = "numeric", #duration of a single time step,in years
    DiscountFactorPerTimeStep = "numeric", #Discouning factor used for the drift
    AdditionOrMuliplicationFlag = "character" #Should script add the difference or multiply by it, hidden variable
  
    ),
  
  validity = function(object){
    if(object@N%%1 != 0 | object@N < 1){
      return("Error: Number of steps needs to be a natural number")
    }
  },
  
  contains = c("Option", "VIRTUAL")
)

setMethod("initialize", "TreeOption",
          function(.Object, ...) {
            .Object <- callNextMethod(.Object, ...)
            validObject(.Object)
            
            .Object@YearsPerTimeStep <- .Object@Years / .Object@N
            .Object@DiscountFactorPerTimeStep <- exp(-(.Object@r)*.Object@YearsPerTimeStep)
            
            return(.Object)
          }
)

BinomialStockOption <- setClass(
  #Basic binomial model
  
  "BinomialStockOption",
  
  slots = c(
    cu = "numeric", #%change in a up move
    cd = "numeric", #%change in a down move
    
    pu = "numeric", #Probability of price changing up, in risk-neutral regime
    pd = "numeric" #Probability of price changing down, in risk-neutral regime
  ),
  
  validity = function(object){
    if(length(object@cu) > 0 & length(object@cd) > 0){
      if(object@cu <= 0 | object@cd <= 0){
        return("Error: You can't have changes in price smaller or equal to 0")
      } else if(length(object@vol) > 0){
        return("Error: Provide either volatility or price changes at nodes")
      }
    } else if(length(object@vol) > 0){
      if(object@vol < 0){
        return("Volatility can't be lower than 0")
      }
    } else if(!(object@VolatilityMethod %in% c("BSE", "Basic"))){
      return("Error: Incorrect volatility method")
    }
  },
  
  contains = "TreeOption"
)

setMethod("initialize", "BinomialStockOption",
          function(.Object, ...) {
            .Object <- callNextMethod(.Object, ...)
            validObject(.Object)
            
            #If volatility is provided, calculate equivalent in changes of the price at each step
            if(length(.Object@vol) > 0){
              .Object@cu <- .Object@vol/sqrt(.Object@N/.Object@Years)/100
              .Object@cd <- .Object@cu
            }
            .Object@pu <- (exp((.Object@r - .Object@div)*.Object@YearsPerTimeStep)-(1-.Object@cd))/((1+.Object@cu)-(1-.Object@cd))
            .Object@pd <- 1-.Object@pu
            .Object@AdditionOrMuliplicationFlag <- "a"
            
            return(.Object)
          }
)

BinomialCCRStockOption <- setClass(
  #Cox-Ross-Rubinstein model
  
  "BinomialCCRStockOption",
  
  slots = c(
    
    cu = "numeric", #%change in a up move
    cd = "numeric", #%change in a down move
    pu = "numeric", #Probability of price changing up, in risk-neutral regime
    pd = "numeric" #Probability of price changing down, in risk-neutral regime
  ),
  
  validity = function(object){
    if(object@vol < 0){
      return("Volatility can't be lower than 0")
    }
  },
  
  contains = "TreeOption"
)

setMethod("initialize", "BinomialCCRStockOption",
          function(.Object, ...) {
            .Object <- callNextMethod(.Object, ...)
            validObject(.Object)
            
            .Object@cu <- exp((.Object@vol/100) * sqrt(.Object@YearsPerTimeStep))
            .Object@cd <- 1/(.Object@cu)
            .Object@pu <- (exp((.Object@r - .Object@div)*.Object@YearsPerTimeStep)-(.Object@cd))/((.Object@cu)-(.Object@cd))
            .Object@pd <- 1-.Object@pu
            .Object@AdditionOrMuliplicationFlag <- "m"
            
            return(.Object)
          }
)

BinomialJRStockOption <- setClass(
  #Jarrow-Rudd model
  
  "BinomialJRStockOption",
  
  slots = c(
    vol = "numeric", #yearly volatility
    
    cu = "numeric", #%change in a up move
    cd = "numeric", #%change in a down move
    pu = "numeric", #Probability of price changing up, in risk-neutral regime
    pd = "numeric" #Probability of price changing down, in risk-neutral regime
  ),
  
  validity = function(object){
    if(object@vol < 0){
      return("Volatility can't be lower than 0")
    }
  },
  
  contains = "TreeOption"
)

setMethod("initialize", "BinomialJRStockOption",
          function(.Object, ...) {
            .Object <- callNextMethod(.Object, ...)
            validObject(.Object)
            
            .Object@cu <- exp((.Object@r - .Object@div - (.Object@vol/100)^2/2) * .Object@YearsPerTimeStep + .Object@vol/100 * sqrt(.Object@YearsPerTimeStep))
            .Object@cd <- exp((.Object@r - .Object@div - (.Object@vol/100)^2/2) * .Object@YearsPerTimeStep - .Object@vol/100 * sqrt(.Object@YearsPerTimeStep))
            .Object@pu <- 0.5
            .Object@pd <- 1-.Object@pu
            .Object@AdditionOrMuliplicationFlag <- "m"
            
            return(.Object)
          }
)

BinomialLRStockOption <- setClass(
  #Leisen-Reimer Model
  
  "BinomialLRStockOption",
  
  slots = c(
    vol = "numeric", #yearly volatility
    
    d1 = "numeric", #Black-Scholes parameter
    d2 = "numeric", #Black-Scholes parameter
    LR = "numeric", #Parameter specific to the LR method
    cu = "numeric", #%change in a up move
    cd = "numeric", #%change in a down move
    pu = "numeric", #Probability of price changing up, in risk-neutral regime
    pd = "numeric" #Probability of price changing down, in risk-neutral regime
  ),
  
  validity = function(object){
    
  },
  
  contains = "TreeOption"
)

setMethod("initialize", "BinomialLRStockOption",
          function(.Object, ...) {
            .Object <- callNextMethod(.Object, ...)
            validObject(.Object)
            
            if(.Object@N%%2 == 0){
              .Object@N <- .Object@N + 1
            }
            .Object@d1 <- (log(.Object@S0/.Object@K) + ((.Object@r - .Object@div) + ((.Object@vol/100)^2)/2)*.Object@Years)/((.Object@vol/100)*sqrt(.Object@Years))
            .Object@d2 <- .Object@d1 - (.Object@vol/100)*sqrt(.Object@Years)
            .Object@LR <- 1/2 + sign(.Object@d1)/2*sqrt(1-exp(-(.Object@d1/(.Object@N+1/3+0.1/(.Object@N+1)))^2*(.Object@N+1/6)))
            .Object@pu <- 1/2 + sign(.Object@d2)/2*sqrt(1-exp(-(.Object@d2/(.Object@N+1/3+0.1/(.Object@N+1)))^2*(.Object@N+1/6)))
            .Object@pd <- 1 - .Object@pu
            .Object@cu <- exp((.Object@r - .Object@div)*.Object@YearsPerTimeStep)*(.Object@LR/.Object@pu)
            .Object@cd <- exp((.Object@r - .Object@div)*.Object@YearsPerTimeStep)*((1-.Object@LR)/(1-.Object@pu))
            .Object@AdditionOrMuliplicationFlag <- "m"
            
            return(.Object)
          }
)

BinomialCCTwoAssetFXOption <- setClass(
  #Cox-Ross-Rubinstein two asset model
  
  "BinomialCCTwoAssetFXOption",
  
  slots = c(
    N = "numeric", #Number of steps
    
    YearsPerTimeStep = "numeric", #duration of a single time step,in years
    DiscountFactorPerTimeStep = "numeric", #Discouning factor used for the drift
    AdditionOrMuliplicationFlag = "character", #Should script add the difference or multiply by it, hidden variable
    cu1 = "numeric", #First asset %change in a up move
    cd1 = "numeric", #First asset %change in a down move
    cu2 = "numeric", #Second asset %change in a up move
    cd2 = "numeric", #Second asset %change in a down move
    puu = "numeric", #Probability of two up moves
    pud = "numeric", #Probability of up-down move
    pdu = "numeric", #Probability of down-up move
    pdd = "numeric" #Probability of two down moves
  ),
  
  validity = function(object){
    
  },
  
  contains = "TwoAssetFXOptions"
)

setMethod("initialize", "BinomialCCTwoAssetFXOption",
          function(.Object, ...) {
            .Object <- callNextMethod(.Object, ...)
            validObject(.Object)
            
            .Object@YearsPerTimeStep <- .Object@Years / .Object@N
            .Object@DiscountFactorPerTimeStep <- exp(-(.Object@r3)*.Object@YearsPerTimeStep)
            
            .Object@cu1 <- exp((.Object@vol1/100) * sqrt(.Object@YearsPerTimeStep))
            .Object@cd1 <- 1/(.Object@cu1)
            .Object@cu2 <- exp((.Object@vol2/100) * sqrt(.Object@YearsPerTimeStep))
            .Object@cd2 <- 1/(.Object@cu2)
            GrowthFactor1 <- 1 - exp(-(.Object@r3 - .Object@r1)*.Object@YearsPerTimeStep)
            GrowthFactor2 <- 1 - exp(-(.Object@r3 - .Object@r2)*.Object@YearsPerTimeStep)
            v1 = GrowthFactor1 - ((.Object@vol1/100)^2)/2
            v2 = GrowthFactor2 - ((.Object@vol2/100)^2)/2
            .Object@puu <- (1+.Object@Corr+sqrt(.Object@YearsPerTimeStep)*(v1/(.Object@vol1/100)+(v2/(.Object@vol2/100))))/4
            .Object@pud <- (1-.Object@Corr+sqrt(.Object@YearsPerTimeStep)*(v1/(.Object@vol1/100)-(v2/(.Object@vol2/100))))/4
            .Object@pdu <- (1-.Object@Corr+sqrt(.Object@YearsPerTimeStep)*(-v1/(.Object@vol1/100)+(v2/(.Object@vol2/100))))/4
            .Object@pdd <- (1+.Object@Corr+sqrt(.Object@YearsPerTimeStep)*(-v1/(.Object@vol1/100)-(v2/(.Object@vol2/100))))/4
            
            return(.Object)
          }
)

setGeneric("GenerateBinomialTree", function(optionName) 
  standardGeneric("GenerateBinomialTree"))

setMethod(f="GenerateBinomialTree", signature="TreeOption", 
          #Probably can be done a lot faster
          #Big trees will take a lot of time and a lot of RAM
          
          definition=function(optionName) {
            if(optionName@flavor == "b"){
              BermudeanMultiplier <- optionName@N/length(optionName@ExerciseDates)
            }
            
            StockMovement <- matrix(nrow = optionName@N+1, ncol = optionName@N+1)
            
            #Create the tree of underlying's movements, either add the price changes or multiply by them
            if(optionName@AdditionOrMuliplicationFlag == "m"){
              for(j in 1:ncol(StockMovement)){
                for(i in 1:j){
                  StockMovement[i, j] <- optionName@S0 * ((optionName@cu)^(j-i)) * (optionName@cd)^(i-1)
                }
              }
            } else if(optionName@AdditionOrMuliplicationFlag == "a"){
              for(j in 1:ncol(StockMovement)){
                for(i in 1:j){
                  StockMovement[i, j] <- optionName@S0 * ((1+optionName@cu)^(j-i)) * (1-optionName@cd)^(i-1)
                }
              }
            }
            
            #Create payout vector based on outcomes and the strike
            if (optionName@flag == "c") {
              PayOut <- matrix(pmax(0, StockMovement - optionName@K), nrow = optionName@N+1, ncol = optionName@N+1)
            } else if (optionName@flag == "p"){
              PayOut <- matrix(pmax(0, optionName@K - StockMovement), nrow = optionName@N+1, ncol = optionName@N+1)
            }
            
            #Barrier option
            if(length(optionName@BarrierType) > 0){
              if(optionName@Barrier > optionName@K){
                PayOut[, ncol(PayOut)] <- ifelse(StockMovement[, ncol(StockMovement)] > optionName@Barrier, 0, PayOut[, ncol(PayOut)])
              } else if(optionName@Barrier < optionName@K){
                PayOut[, ncol(PayOut)] <- ifelse(StockMovement[, ncol(StockMovement)] < optionName@Barrier, 0, PayOut[, ncol(PayOut)])
              }
            }
            
            #Starting from the end, calculate the nodes values, taking into account the probabilities of up and down moves
            MidNodeRevenue <- matrix(nrow = optionName@N+1, ncol = optionName@N+1)
            MidNodeRevenue[, ncol(MidNodeRevenue)] <- PayOut[, ncol(PayOut)]
            for(i in (ncol(MidNodeRevenue)-1):1){
              
              for(k in 1:i){
                MidNodeRevenue[k, i] <- (MidNodeRevenue[k, i+1] * optionName@pu + MidNodeRevenue[k+1, i+1] * optionName@pd) * optionName@DiscountFactorPerTimeStep
                }
              
              #If the option is american, on every step of tree's depth the tree's value is either current value of exercising the option or the future possible values
              if(optionName@flavor == "a"){
                MidNodeRevenue[, i] <- pmax(MidNodeRevenue[, i], PayOut[, i])
              } else if(optionName@flavor == "b"){
                if((i-1)%%BermudeanMultiplier == 0){
                  MidNodeRevenue[, i] <- pmax(MidNodeRevenue[, i], PayOut[, i])
                }
              }
              
              #Barrier option
              if(length(optionName@BarrierType) > 0){
                if(optionName@BarrierType == "KO" | optionName@flavor == "e"){
                  if(optionName@Barrier > optionName@K){
                    MidNodeRevenue[, i] <- ifelse(StockMovement[, i] > optionName@Barrier, 0, MidNodeRevenue[, i])
                  } else if(optionName@Barrier < optionName@K){
                    MidNodeRevenue[, i] <- ifelse(StockMovement[, i] < optionName@Barrier, 0, MidNodeRevenue[, i])
                  }
                }
              }
            }
            if(optionName@flavor == "e"){
              if(length(optionName@BarrierType) > 0){
                if(optionName@BarrierType == "KI"){
                  VanillaTree <- GenerateBinomialTree(new(class(optionName)[1], optionName, Barrier = numeric(0), BarrierType = character(0)))
                  MidNodeRevenueVanilla <- VanillaTree[[2]]
                  MidNodeRevenue <- MidNodeRevenueVanilla - MidNodeRevenue
                }
              }
            }
            
            OutputList <- list(StockMovement, MidNodeRevenue, PayOut)
            return(OutputList)
          }
)

setGeneric("FastBinomialEuropeanStockOptionPrices", function(optionName) 
  standardGeneric("FastBinomialEuropeanStockOptionPrices"))

setMethod(f="FastBinomialEuropeanStockOptionPrices", signature="TreeOption", 
          #Method does not calculate the entire tree, only the end nodes are needed for European Option
          #Much faster, but only work for European vanilla options
          
          definition=function(optionName) {
            EndNodeRevenue <- c()
            #Create a vector of possible outcomes based on binomial distribution given the probability
            ProbabilityTable <- dbinom(seq(0, optionName@N, 1), optionName@N, optionName@pd)
            
            #Depending on the flag, either add the price change or multiply it
            if(optionName@AdditionOrMuliplicationFlag == "m"){
              for(i in 0:optionName@N){
                EndNodeRevenue[i+1] <- optionName@S0 * ((optionName@cu)^(optionName@N-i)) * (optionName@cd)^i
              }
            } else if(optionName@AdditionOrMuliplicationFlag == "a") {
              for(i in 0:optionName@N){
                EndNodeRevenue[i+1] <- optionName@S0 * ((1+optionName@cu)^(optionName@N-i)) * (1-optionName@cd)^i
              }
            }
            
            #Create payout vector based on outcomes and the strike
            if (optionName@flag == "c") {
              PayOut <- pmax(0, EndNodeRevenue - optionName@K)
            } else if (optionName@flag == "p"){
              PayOut <- pmax(0, optionName@K - EndNodeRevenue)
            }
            
            #Discount the payouts
            PayOut <- PayOut * optionName@DiscountFactor
            
            #The price is a sum of discounted payouts times the probability of these payouts
            optionName@p <- sum(PayOut * ProbabilityTable)
            
            #Overrides a price in the global environment
            nameObject <- deparse(substitute(MyOption))
            assign(nameObject, optionName, envir=parent.frame())
            return(optionName@p)
          }
)

setGeneric("BinomialStockOptionPrices", function(optionName) 
  standardGeneric("BinomialStockOptionPrices"))

setMethod(f="BinomialStockOptionPrices", signature="TreeOption",
          
          definition=function(optionName) {
            
            TreeList <- GenerateBinomialTree(optionName)
            StockMovement <- TreeList[[1]]
            MidNodeRevenue <- TreeList[[2]]
            PayOut <- TreeList[[3]]
            
            #The price is the first value in the tree
            optionName@p <- MidNodeRevenue[1, 1]
            
            #Overrides a price in the global environment
            nameObject <- deparse(substitute(MyOption))
            assign(nameObject, optionName, envir=parent.frame())
            return(optionName@p)
          }
)

setGeneric("TreeGraph", function(optionName, dx = -0.025, dy = 0.3, cex = 1, digits = 2, GraphType, ...) 
  standardGeneric("TreeGraph"))

setMethod(f="TreeGraph", signature="TreeOption",
          
          definition=function(optionName, dx = -0.025, dy = 0.3, cex = 1, digits = 2, GraphType, ...) {
            #dx moves the text in the x direction, dy in the y direction, cex is scaling multiplier and digits is a rounding parameter
            #GraphType tells what should be graphed, either underlying's movements, the values of the option or the payouts of the option
            
            TreeList <- GenerateBinomialTree(optionName)
            StockMovement <- TreeList[[1]]
            MidNodeRevenue <- TreeList[[2]]
            PayOut <- TreeList[[3]]
            
            #Select what to graph based on flag
            if(GraphType == "underlying"){
              Tree = round(StockMovement, digits = digits)
              GraphTitle <- "Underlying movement"
            } else if(GraphType == "value"){
              Tree = round(MidNodeRevenue, digits = digits)
              GraphTitle <- "Option value"
            } else if(GraphType == "payout"){
              Tree = round(PayOut, digits = digits)
              GraphTitle <- "Payout of the option"
            }
            
            #Create plot's background
            depth = ncol(Tree)
            plot(x = c(0, depth-1), y = c(-depth + 1, depth-0.5), type = "n", 
                 col = 0, xlab = "Depth of the tree", ylab = "Height of the tree")
            
            #Add probabilities to the chart
            text(x = 0.25, y = depth - 1, paste("Probability up:", round(optionName@pu, digits)), cex = cex)
            text(x = 0.3, y = depth - 1 - 0.75, paste("Probability down:", round(optionName@pd, digits)), cex = cex)
            
            #Add the first point
            points(x = 0, y = 0)
            text(0 + dx, 0 + dy, deparse(Tree[1, 1]), cex = cex)
            
            #Add title
            title(GraphTitle)
            
            #Add the rest of the points
            for (i in 1:(depth - 1)) {
              y = seq(from = -i, by = 2, length = i + 1)
              x = rep(i, times = length(y)) + 0
              points(x, y, col = 1)
              for (j in 1:length(x)) text(x[j] + dx, y[j] + dy, deparse(Tree[length(x) + 1 - j, i + 1]), cex = cex)
              y = (-i):i
              x = rep(c(i, i-1), times = 2 * i)[1:length(y)]
              lines(x, y, col = 2)
            }
          }
)

setGeneric("BinomialStockOptionGreeks", function(optionName, digits = 4, method) 
  standardGeneric("BinomialStockOptionGreeks"))

setMethod(f="BinomialStockOptionGreeks", signature="TreeOption",
          
          definition=function(optionName, digits = 4, method) {
            
            TreeList <- GenerateBinomialTree(optionName)
            StockMovement <- TreeList[[1]]
            MidNodeRevenue <- TreeList[[2]]
            PayOut <- TreeList[[3]]
            
            if(method == "fast"){
              #Fast method uses some clever tricks to calculate greeks, but only works if the tree is centered around the starting price
              
              if(round(StockMovement[1, 1], 4) != round(StockMovement[2, 3], 4)){
                print("Warning: Tree is not centered aroound the starting price, which means the incorrect method was used. Greeks won't be correct!")
              }
              
              OptionDelta <- (MidNodeRevenue[1, 2] - MidNodeRevenue[2, 2]) / (StockMovement[1, 2] - StockMovement[2, 2])
              hGamma <- (StockMovement[1, 3] - StockMovement[3, 3])/2
              OptionGamma <- (((MidNodeRevenue[1, 3] - MidNodeRevenue[2, 3]) / (StockMovement[1, 3] - StockMovement[1, 1])) - ((MidNodeRevenue[2, 3] - MidNodeRevenue[3, 3]) / (StockMovement[1, 1] - StockMovement[3, 3]))) / hGamma
              OptionTheta <- (MidNodeRevenue[2, 3] - MidNodeRevenue[1, 1]) / (2 * 365 * optionName@YearsPerTimeStep)
              OptionVega <- MidNodeRevenue[1, 1] - BinomialStockOptionPrices(new(class(optionName)[1], optionName, vol = optionName@vol - 1))
              OptionRho <- MidNodeRevenue[1, 1] - BinomialStockOptionPrices(new(class(optionName)[1], optionName, r = optionName@r - 0.01))
              print(paste("Delta:", round(OptionDelta, digits)))
              print(paste("Gamma:", round(OptionGamma, digits)))
              print(paste("Theta:", round(OptionTheta, digits)))
              print(paste("Vega:", round(OptionVega, digits)))
              print(paste("Rho:", round(OptionRho, digits)))
            } else if(method == "slow"){
              #Slow method uses brute force to calculate greeks
              
              OptionDelta <- (-BinomialStockOptionPrices(new(class(optionName)[1], optionName, S0 = optionName@S0 - 1)) + BinomialStockOptionPrices(new(class(optionName)[1], optionName, S0 = optionName@S0 + 1)))/2
              OptionGamma <- ((OptionDelta - ((-BinomialStockOptionPrices(new(class(optionName)[1], optionName, S0 = optionName@S0 - 2)) + BinomialStockOptionPrices(new(class(optionName)[1], optionName, S0 = optionName@S0)))/2)) - (OptionDelta - ((-BinomialStockOptionPrices(new(class(optionName)[1], optionName, S0 = optionName@S0)) + BinomialStockOptionPrices(new(class(optionName)[1], optionName, S0 = optionName@S0 + 2)))/2)))/2
              OptionTheta <- BinomialStockOptionPrices(new(class(optionName)[1], optionName, Years = optionName@Years - 1/365)) - MidNodeRevenue[1, 1]
              OptionVega <- MidNodeRevenue[1, 1] - BinomialStockOptionPrices(new(class(optionName)[1], optionName, vol = optionName@vol - 1))
              OptionRho <- MidNodeRevenue[1, 1] - BinomialStockOptionPrices(new(class(optionName)[1], optionName, r = optionName@r - 0.01))
              print(paste("Delta:", round(OptionDelta, digits)))
              print(paste("Gamma:", round(OptionGamma, digits)))
              print(paste("Theta:", round(OptionTheta, digits)))
              print(paste("Vega:", round(OptionVega, digits)))
              print(paste("Rho:", round(OptionRho, digits)))
            }
          }
)

setGeneric("MonteCarloOnATree", function(optionName, MonteCarloN) 
  standardGeneric("MonteCarloOnATree"))

setMethod(f="MonteCarloOnATree", signature="TreeOption",
          
          definition=function(optionName, MonteCarloN) {
            
            MonteCarloMarix <- matrix(nrow = MonteCarloN, ncol = optionName@N)
            for(i in 1:nrow(MonteCarloMarix)){
              MonteCarloMarix[i,] <- runif(n = optionName@N, min = 0, max = 1)
              MonteCarloMarix[i,] <- ifelse(MonteCarloMarix[i,] <= optionName@pu, optionName@cu, optionName@cd)
              MonteCarloMarix[i, 1] <- MonteCarloMarix[i, 1] * optionName@S0
              for(j in 2:ncol(MonteCarloMarix)){
                MonteCarloMarix[i, j] <- MonteCarloMarix[i, j-1] * MonteCarloMarix[i, j]
              }
            }
            if(optionName@flavor == "e")(
              if(optionName@flag == "c"){
                PayOut <- pmax(MonteCarloMarix[, ncol(MonteCarloMarix)] - optionName@K, 0)
              } else if(optionName@flag == "p"){
                PayOut <- pmax(optionName@K - MonteCarloMarix[, ncol(MonteCarloMarix)], 0)
              }
            )
            
            if(length(optionName@BarrierType) > 0){
              if(optionName@BarrierType == "KO"){
                for(i in 1:nrow(MonteCarloMarix)){
                  if(optionName@Barrier > optionName@K){
                    if(any(MonteCarloMarix[i, ] > optionName@Barrier)){
                      PayOut[i] <- 0
                    }
                  } else if(optionName@Barrier < optionName@K){
                    if(any(MonteCarloMarix[i, ] < optionName@Barrier)){
                      PayOut[i] <- 0
                    }
                  }
                }
              } else if(optionName@BarrierType == "KI"){
                for(i in 1:nrow(MonteCarloMarix)){
                  if(optionName@Barrier > optionName@K){
                    if(all(MonteCarloMarix[i, ] < optionName@Barrier)){
                      PayOut[i] <- 0
                    }
                  } else if(optionName@Barrier < optionName@K){
                    if(all(MonteCarloMarix[i, ] > optionName@Barrier)){
                      PayOut[i] <- 0
                    }
                  }
                }
              }
            }
            
            PayOut <- PayOut * optionName@DiscountFactor
            optionName@p <- mean(PayOut)
            
            #Overrides a price in the global environment
            nameObject <- deparse(substitute(MyOption))
            assign(nameObject, optionName, envir=parent.frame())
            return(optionName@p)
          }
)

setGeneric("MonteCarloOnATreeTwoAssets", function(optionName, MonteCarloN) 
  standardGeneric("MonteCarloOnATreeTwoAssets"))

setMethod(f="MonteCarloOnATreeTwoAssets", signature="BinomialCCTwoAssetFXOption", 
          
          definition=function(optionName, MonteCarloN) {
            
            MonteCarloMarix1 <- matrix(nrow = MonteCarloN, ncol = optionName@N)
            MonteCarloMarix2 <- matrix(nrow = MonteCarloN, ncol = optionName@N)
            MonteCarloMarix1[, 1] <- optionName@S01
            MonteCarloMarix2[, 1] <- optionName@S02
            for(i in 1:nrow(MonteCarloMarix1)){
              for(j in 2:ncol(MonteCarloMarix1)){
                RandomNumber <- runif(1)
                if(RandomNumber <= optionName@puu){
                  MonteCarloMarix1[i, j] <- MonteCarloMarix1[i, j-1] * optionName@cu1
                  MonteCarloMarix2[i, j] <- MonteCarloMarix2[i, j-1] * optionName@cu2
                } else if(RandomNumber >= optionName@puu & RandomNumber < (optionName@puu + optionName@pud)){
                  MonteCarloMarix1[i, j] <- MonteCarloMarix1[i, j-1] * optionName@cu1
                  MonteCarloMarix2[i, j] <- MonteCarloMarix2[i, j-1] * optionName@cd2
                } else if(RandomNumber >= (optionName@puu + optionName@pud) & RandomNumber < (optionName@puu + optionName@pud + optionName@pdu)){
                  MonteCarloMarix1[i, j] <- MonteCarloMarix1[i, j-1] * optionName@cd1
                  MonteCarloMarix2[i, j] <- MonteCarloMarix2[i, j-1] * optionName@cu2
                } else if(RandomNumber >= (optionName@puu + optionName@pud + optionName@pdu)){
                  MonteCarloMarix1[i, j] <- MonteCarloMarix1[i, j-1] * optionName@cd1
                  MonteCarloMarix2[i, j] <- MonteCarloMarix2[i, j-1] * optionName@cd2
                }
              }
            }
            
            if(optionName@flavor == "e"){
              if(optionName@flag1 == "c"){
                PayOut1 <- pmax(MonteCarloMarix1[, ncol(MonteCarloMarix1)] - optionName@K1, 0)
              } else if(optionName@flag1 == "p"){
                PayOut1 <- pmax(optionName@K1 - MonteCarloMarix1[, ncol(MonteCarloMarix1)], 0)
              }
              if(optionName@flag2 == "c"){
                PayOut2 <- pmax(MonteCarloMarix2[, ncol(MonteCarloMarix2)] - optionName@K2, 0)
              } else if(optionName@flag2 == "p"){
                PayOut2 <- pmax(optionName@K2 - MonteCarloMarix2[, ncol(MonteCarloMarix2)], 0)
              }
            }
            
            # if(length(optionName@BarrierType) > 0){
            #   if(optionName@BarrierType == "KO"){
            #     for(i in 1:nrow(MonteCarloMarix)){
            #       if(optionName@Barrier > optionName@K){
            #         if(any(MonteCarloMarix[i, ] > optionName@Barrier)){
            #           PayOut[i] <- 0
            #         }
            #       } else if(optionName@Barrier < optionName@K){
            #         if(any(MonteCarloMarix[i, ] < optionName@Barrier)){
            #           PayOut[i] <- 0
            #         }
            #       }
            #     }
            #   } else if(optionName@BarrierType == "KI"){
            #     for(i in 1:nrow(MonteCarloMarix)){
            #       if(optionName@Barrier > optionName@K){
            #         if(all(MonteCarloMarix[i, ] < optionName@Barrier)){
            #           PayOut[i] <- 0
            #         }
            #       } else if(optionName@Barrier < optionName@K){
            #         if(all(MonteCarloMarix[i, ] > optionName@Barrier)){
            #           PayOut[i] <- 0
            #         }
            #       }
            #     }
            #   }
            #}
            
            PayOut <- vector("numeric", length(PayOut1))
            for(i in 1:length(PayOut1)){
              if(PayOut1[i] < PayOut2[i]){
                PayOut[i] <- PayOut1[i]
              } else {
                PayOut[i] <- PayOut2[i]
              }
            }
            
            PayOut <- PayOut * optionName@DiscountFactor
            optionName@p <- mean(PayOut)
            
            #Overrides a price in the global environment
            nameObject <- deparse(substitute(MyOption))
            assign(nameObject, optionName, envir=parent.frame())
            return(optionName@p)
          }
)
