Option <- setClass(
  
  "Option",
  
  slots = c(
    S0 = "numeric", #Stock Price
    K = "numeric", #Strike
    r = "numeric", #Risk Free Rate during the live of the Option
    cD = "numeric", #Context Date in yyyymmdd format
    mD = "numeric", #Maturity Date in yyyymmdd format
    div = "numeric", #dividend yield
    flag = "character", #c for call, p for put
    flavor = "character", #e for European, a for American
    
    Years = "numeric", #time to maturity in years
    DiscountFactorPerTimeStep = "numeric" #discouning factor used for drift
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
            .Object@YearsPerTimesstep <- .Object@Years / .Object@N
            .Object@DiscountFactorPerTimeStep <- exp(-(.Object@r - .Object@div)*.Object@YearsPerTimesstep)
            return(.Object)
          }
)

TreeEuropeanOption <- setClass(
  
  "TreeEuropeanOption",
  
  slots = c(
    N = "numeric", #Number of steps
    
    YearsPerTimesstep = "numeric" #duration of a single time step,in years
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
            .Object@YearsPerTimesstep <- .Object@Years / .Object@N
            .Object@DiscountFactorPerTimeStep <- exp(-(.Object@r - .Object@div)*.Object@YearsPerTimesstep)
            return(.Object)
          }
)

BinomialEuropeanOption <- setClass(
  
  "BinomialEuropeanOption",
  
  slots = c(
    pu = "numeric", #%change in a up move
    pd = "numeric" #%change in a down move
  ),
  
  validity = function(object){
    if(object@pu <= 0 | object@pd <= 0){
      return("Error: You can't have probabilities smaller or equal to 0")
    }
  },
  
  contains = "TreeEuropeanOption"
)
