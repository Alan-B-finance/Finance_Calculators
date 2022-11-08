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
    flavor = "character", #e for European, a for American
    p = "numeric", #Price of the option
    
    Years = "numeric", #time to maturity in years
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
    } else if(nchar(object@cD) != 8 | nchar(object@mD) != 8){
      return("Error: Please provide dates in the yyyymmdd format")
    } else if(!(object@flag %in% c("c", "p"))){
      return("Error: The flag can either be c (call) or p (put)")
    } else if(!(object@flavor %in% c("e", "a"))){
      return("Error: The flag can either be e (european) or a (american)")
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
            
            .Object@Years <- as.numeric(as.Date(as.character(.Object@mD), format("%Y%m%d")) - as.Date(as.character(.Object@cD), format("%Y%m%d")))/365
            .Object@DiscountFactor <- 1/exp(.Object@r - .Object@div)^(.Object@Years)
            
            return(.Object)
          }
)

TreeEuropeanOption <- setClass(
  
  "TreeEuropeanOption",
  
  slots = c(
    N = "numeric", #Number of steps
    
    YearsPerTimeStep = "numeric" #duration of a single time step,in years
  ),
  
  validity = function(object){
    if(object@N%%1 != 0 | object@N < 1){
      return("Error: Number of steps needs to be a natural number")
    }
  },
  
  contains = c("Option", "VIRTUAL")
)

setMethod("initialize", "TreeEuropeanOption",
          function(.Object, ...) {
            .Object <- callNextMethod(.Object, ...)
            validObject(.Object)
            
            .Object@YearsPerTimeStep <- .Object@Years / .Object@N
            
            return(.Object)
          }
)

BinomialEuropeanOption <- setClass(
  
  "BinomialEuropeanOption",
  
  slots = c(
    cu = "numeric", #%change in a up move
    cd = "numeric", #%change in a down move
    
    pu = "numeric", #Probability of price changing up, in risk-neutral regime
    pd = "numeric" #Probability of price changing down, in risk-neutral regime
  ),
  
  validity = function(object){
    if(object@cu <= 0 | object@cd <= 0){
      return("Error: You can't have changes in price smaller or equal to 0")
    }
  },
  
  contains = "TreeEuropeanOption"
)

setMethod("initialize", "BinomialEuropeanOption",
          function(.Object, ...) {
            .Object <- callNextMethod(.Object, ...)
            validObject(.Object)
            
            .Object@pu <- (exp((.Object@r - .Object@div)*.Object@YearsPerTimeStep)-(1-.Object@cd))/((1+.Object@cu)-(1-.Object@cd))
            .Object@pd <- 1-.Object@pu
            
            return(.Object)
          }
)

setGeneric("BinomialEuropeanOptionStockPrices", function(optionName) 
  standardGeneric("BinomialEuropeanOptionStockPrices") )

setMethod(f="BinomialEuropeanOptionStockPrices", signature="BinomialEuropeanOption", 
          #Method does not calculate the entire tree, only the end nodes are needed for European Option
          
          definition=function(optionName) {
            EndNodeRevenue <- c()
            ProbabilityTable <- dbinom(seq(0, optionName@N, 1), optionName@N, optionName@pd)
            for(i in 0:optionName@N){
              EndNodeRevenue[i+1] <- optionName@S0 * ((1+optionName@cu)^(optionName@N-i)) * (1-optionName@cd)^i
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

BinomialAmericanOption <- setClass(
  
  "BinomialAmericanOption",
  
  slots = c(
    cu = "numeric", #%change in a up move
    cd = "numeric", #%change in a down move
    
    pu = "numeric", #Probability of price changing up, in risk-neutral regime
    pd = "numeric", #Probability of price changing down, in risk-neutral regime
    YearsPerTimesstep = "numeric", #Duration of a single time step,in years
    DiscountFactorPerTimeStep = "numeric" #Discouning factor used for drift
  ),
  
  validity = function(object){
    if(object@cu <= 0 | object@cd <= 0){
      return("Error: You can't have changes in price smaller or equal to 0")
    }
  },
  
  contains = "TreeEuropeanOption"
)

setMethod("initialize", "BinomialAmericanOption",
          function(.Object, ...) {
            .Object <- callNextMethod(.Object, ...)
            validObject(.Object)
            
            .Object@pu <- (exp((.Object@r - .Object@div)*.Object@YearsPerTimeStep)-(1-.Object@cd))/((1+.Object@cu)-(1-.Object@cd))
            .Object@pd <- 1-.Object@pu
            .Object@YearsPerTimesstep <- .Object@Years / .Object@N
            .Object@DiscountFactorPerTimeStep <- exp(-(.Object@r - .Object@div)*.Object@YearsPerTimesstep)
            
            return(.Object)
          }
)

setGeneric("BinomialAmericanOptionStockPrices", function(optionName) 
  standardGeneric("BinomialAmericanOptionStockPrices") )

setMethod(f="BinomialAmericanOptionStockPrices", signature="BinomialAmericanOption", 
          definition=function(optionName) {
            StockMovement <- matrix(nrow = optionName@N+1, ncol = optionName@N+1)
            EndNodeRevenue <- c()
            for(j in 1:ncol(StockMovement)){
              for(i in 1:j){
                StockMovement[i, j] <- optionName@S0 * ((1+optionName@cu)^(j-i)) * (1-optionName@cd)^(i-1)# / optionName@DiscountFactorPerTimeStep^j
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
              MidNodeRevenue[, i] <- pmax(MidNodeRevenue[, i], PayOut[, i])
            }
            
            optionName@p <- MidNodeRevenue[1, 1]
            nameObject <- deparse(substitute(MyOption))
            assign(nameObject, optionName, envir=parent.frame()) #Overrides a price in the global environment
            return(optionName@p)
          }
)
