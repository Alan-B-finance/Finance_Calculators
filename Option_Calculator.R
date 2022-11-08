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
    TP = "numeric", #Vector of terminal prices
    
    pu = "numeric", #Probability of price changing up, in risk-neutral regime
    pd = "numeric" #Probability of price changing down, in risk-neutral regime
  ),
  
  validity = function(object){
    if(object@cu <= 0 | object@cd <= 0){
      return("Error: You can't have changes in prices smaller or equal to 0")
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
          definition=function(optionName) {
            EndNodeRevenue <- c()
            ProbabilityTable <- c()
            for(i in 0:optionName@N){
              EndNodeRevenue[i+1] <- optionName@S0 * ((1+optionName@cu)^(optionName@N-i)) * (1-optionName@cd)^i
              ProbabilityTable[i+1] <- factorial(optionName@N)/(factorial(i)*factorial(optionName@N-i)) * optionName@pu^(optionName@N-i)*optionName@pd^i
            }
            if (optionName@flag == "c") {
              PayOut <- pmax(0, EndNodeRevenue - optionName@K)
            } else if (optionName@flag == "p"){
              PayOut <- pmax(0, optionName@K - EndNodeRevenue)
            }
            PayOut <- PayOut * optionName@DiscountFactor
            optionName@p <- sum(PayOut * ProbabilityTable)
            nameObject <- deparse(substitute(MyOption))
            assign(nameObject, optionName, envir=parent.frame())
            return(sum(PayOut * ProbabilityTable))
        }
)
