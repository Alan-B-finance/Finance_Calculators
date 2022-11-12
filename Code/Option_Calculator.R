Option <- setClass(
  
  "Option",
  
  slots = c(
    S0 = "numeric", #Stock Price
    K = "numeric", #Strike
    r = "numeric", #Risk Free Rate during the live of the Option
    cD = "numeric", #Context Date in yyyymmdd format
    mD = "numeric", #Maturity Date in yyyymmdd format
    div = "numeric", #Dividend yield
    flag = "character", #c for call, p for put
    vol = "numeric", #yearly volatility
    p = "numeric", #Price of the option
    Years = "numeric", #time to maturity in years
    flavor = "character", #a for american, e for european
    
    DiscountFactor = "numeric" #Overall discount Factor to maturity
  ),
  
  prototype = list(
    div = 0
  ),
  
  validity = function(object){
    if(object@S0 < 0){
      return("Error: Price can't be lower than 0")
    } else if(object@K < 0){
      return("Error: Barrier can't be lower than 0")
    } else if(object@r < 0){
      return("Error: Risk free rate can't be lower than 0")
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
    }
  },
  
  contains=c("VIRTUAL")
)

setMethod("initialize", "Option",
          function(.Object, ...) {
            .Object <- callNextMethod(.Object, ...)
            validObject(.Object)
            
            if(length(.Object@Years) == 0){
              .Object@Years <- as.numeric(as.Date(as.character(.Object@mD), format("%Y%m%d")) - as.Date(as.character(.Object@cD), format("%Y%m%d")))/365
            }
            .Object@DiscountFactor <- 1/exp(.Object@r - .Object@div)^(.Object@Years)
            
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
            .Object@DiscountFactorPerTimeStep <- exp(-(.Object@r - .Object@div)*.Object@YearsPerTimeStep)
            
            return(.Object)
          }
)

BinomialStockOption <- setClass(
  
  "BinomialStockOption",
  
  slots = c(
    cu = "numeric", #%change in a up move
    cd = "numeric", #%change in a down move
    vol = "numeric", #yearly volatility
    
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
    } else if(!(object@VolatilityMethod %in% c("BSE", "CCR", "Basic"))){
      return("Error: Incorrect volatility method")
    }
  },
  
  contains = "TreeOption"
)

setMethod("initialize", "BinomialStockOption",
          function(.Object, ...) {
            .Object <- callNextMethod(.Object, ...)
            validObject(.Object)
            
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
  
  "BinomialCCRStockOption",
  
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

setGeneric("FastBinomialEuropeanStockOptionPrices", function(optionName) 
  standardGeneric("FastBinomialEuropeanStockOptionPrices"))

setMethod(f="FastBinomialEuropeanStockOptionPrices", signature="TreeOption", 
          #Method does not calculate the entire tree, only the end nodes are needed for European Option
          
          definition=function(optionName) {
            EndNodeRevenue <- c()
            ProbabilityTable <- dbinom(seq(0, optionName@N, 1), optionName@N, optionName@pd)
            if(optionName@AdditionOrMuliplicationFlag == "m"){
              for(i in 0:optionName@N){
                EndNodeRevenue[i+1] <- optionName@S0 * ((optionName@cu)^(optionName@N-i)) * (optionName@cd)^i
              }
            } else if(optionName@AdditionOrMuliplicationFlag == "a") {
              for(i in 0:optionName@N){
                EndNodeRevenue[i+1] <- optionName@S0 * ((1+optionName@cu)^(optionName@N-i)) * (1-optionName@cd)^i
              }
            }
            if (optionName@flag == "c") {
              PayOut <- pmax(0, EndNodeRevenue - optionName@K)
            } else if (optionName@flag == "p"){
              PayOut <- pmax(0, optionName@K - EndNodeRevenue)
            }
            PayOut <- PayOut * optionName@DiscountFactor
            optionName@p <- sum(PayOut * ProbabilityTable)
            nameObject <- deparse(substitute(MyOption))
            assign(nameObject, optionName, envir=parent.frame()) #Overrides a price in the global environment
            return(optionName@p)
          }
)

setGeneric("BinomialStockOptionPrices", function(optionName) 
  standardGeneric("BinomialStockOptionPrices"))

setMethod(f="BinomialStockOptionPrices", signature="TreeOption",
          #Probably can be done a lot faster
          #Big trees will take a lot of time and a lot of RAM
          definition=function(optionName) {
            StockMovement <- matrix(nrow = optionName@N+1, ncol = optionName@N+1)
            if(optionName@VolatilityMethod == "CCR"){
              for(j in 1:ncol(StockMovement)){
                for(i in 1:j){
                  StockMovement[i, j] <- optionName@S0 * ((optionName@cu)^(j-i)) * (optionName@cd)^(i-1)
                }
              }
            } else {
              for(j in 1:ncol(StockMovement)){
                for(i in 1:j){
                  StockMovement[i, j] <- optionName@S0 * ((1+optionName@cu)^(j-i)) * (1-optionName@cd)^(i-1)
                }
              }
            }
            if (optionName@flag == "c") {
              PayOut <- matrix(pmax(0, StockMovement - optionName@K), nrow = optionName@N+1, ncol = optionName@N+1)
            } else if (optionName@flag == "p"){
              PayOut <- matrix(pmax(0, optionName@K - StockMovement), nrow = optionName@N+1, ncol = optionName@N+1)
            }
            
            MidNodeRevenue <- matrix(nrow = optionName@N+1, ncol = optionName@N+1)
            MidNodeRevenue[, ncol(MidNodeRevenue)] <- PayOut[, ncol(PayOut)]
            for(i in (ncol(MidNodeRevenue)-1):1){
              for(k in 1:i){
                MidNodeRevenue[k, i] <- (MidNodeRevenue[k, i+1] * optionName@pu + MidNodeRevenue[k+1, i+1] * optionName@pd) * optionName@DiscountFactorPerTimeStep
              }
              if(optionName@flavor == "a"){
                MidNodeRevenue[, i] <- pmax(MidNodeRevenue[, i], PayOut[, i])
              }
            }
            
            optionName@p <- MidNodeRevenue[1, 1]
            nameObject <- deparse(substitute(MyOption))
            assign(nameObject, optionName, envir=parent.frame()) #Overrides a price in the global environment
            return(optionName@p)
          }
)

BinomialLRStockOption <- setClass(
  
  "BinomialLRStockOption",
  
  slots = c(
    vol = "numeric", #yearly volatility
    
    d1 = "numeric",
    d2 = "numeric",
    LR = "numeric",
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

setGeneric("TreeGraph", function(optionName, dx = -0.025, dy = 0.3, cex = 1, digits = 2, GraphType, ...) 
  standardGeneric("TreeGraph"))

setMethod(f="TreeGraph", signature="TreeOption",
          #Probably can be done a lot faster
          #Big trees will take a lot of time and a lot of RAM
          definition=function(optionName, dx = -0.025, dy = 0.3, cex = 1, digits = 2, GraphType, ...) {
            StockMovement <- matrix(nrow = optionName@N+1, ncol = optionName@N+1)
            if(optionName@AdditionOrMuliplicationFlag == "m"){
              for(j in 1:ncol(StockMovement)){
                for(i in 1:j){
                  StockMovement[i, j] <- optionName@S0 * ((optionName@cu)^(j-i)) * (optionName@cd)^(i-1)
                }
              }
            } else if(optionName@AdditionOrMuliplicationFlag == "a") {
              for(j in 1:ncol(StockMovement)){
                for(i in 1:j){
                  StockMovement[i, j] <- optionName@S0 * ((1+optionName@cu)^(j-i)) * (1-optionName@cd)^(i-1)
                }
              }
            }
            if (optionName@flag == "c") {
              PayOut <- matrix(pmax(0, StockMovement - optionName@K), nrow = optionName@N+1, ncol = optionName@N+1)
            } else if (optionName@flag == "p"){
              PayOut <- matrix(pmax(0, optionName@K - StockMovement), nrow = optionName@N+1, ncol = optionName@N+1)
            }
            
            MidNodeRevenue <- matrix(nrow = optionName@N+1, ncol = optionName@N+1)
            MidNodeRevenue[, ncol(MidNodeRevenue)] <- PayOut[, ncol(PayOut)]
            for(i in (ncol(MidNodeRevenue)-1):1){
              for(k in 1:i){
                MidNodeRevenue[k, i] <- (MidNodeRevenue[k, i+1] * optionName@pu + MidNodeRevenue[k+1, i+1] * optionName@pd) * optionName@DiscountFactorPerTimeStep
              }
              if(optionName@flavor == "a"){
                MidNodeRevenue[, i] <- pmax(MidNodeRevenue[, i], PayOut[, i])
              }
            }
            
            if(GraphType == "underlying"){
              Tree = round(StockMovement, digits = digits)
            } else if(GraphType == "Value"){
              Tree = round(MidNodeRevenue, digits = digits)
            } else if(GraphType == "payout"){
              Tree = round(PayOut, digits = digits)
            } 
            depth = ncol(Tree)
            plot(x = c(0, depth-1), y = c(-depth + 1, depth-0.5), type = "n", 
                 col = 0, ...)
            points(x = 0, y = 0)
            text(0 + dx, 0 + dy, deparse(Tree[1, 1]), cex = cex)
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
