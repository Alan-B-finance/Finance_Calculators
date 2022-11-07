Option <- setClass(
  
  "Option",
  
  slots = c(
    S0 = "numeric", #Stock Price
    K = "numeric", #Strike
    r = "numeric", #Risk Free Rate during the live of the Option
    cD = "numeric", #Context Date in yyyymmdd format
    mD = "numeric", #Maturity Date in yyyymmdd format
    N = "numeric", #Number of steps
    pu = "numeric", #Probability of up move
    pd = "numeric", #Probability of down move
    div = "numeric", #dividend yield
    flag = "character", #c for call, p for put
    flavor = "character", #e for European, a for American
    
    years = "numeric" #time to maturity in years
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
    } else if(object@N%%1 != 0 | object@N < 1){
      return("Error: Number of steps needs to be a natural number")
    } else if(length(object@pu) != 0 & length(object@pd) != 0){
      if(object@pu + object@pd != 1){
        return("Error: The sum of probabilities needs to be 1")
      } else if(object@pu < 0 | object@pd < 0){
        return("Error: You can't have negtive probabilities")
      } 
    } else if(!(object@flag %in% c("c", "p"))){
      return("Error: The flag can either be c (call) or p (put)")
    } else if(!(object@flavor %in% c("e", "a"))){
      return("Error: The flag can either be e (european) or a (american)")
    }
  }
)

setGeneric(name="CalculateTime", def=function(optionName){
  standardGeneric("CalculateTime")
  }
)

setMethod("initialize", "Option",
          function(.Object, ...) {
            .Object <- callNextMethod(.Object, ...)
            validObject(.Object)
            .Object@years <- as.numeric(as.Date(as.character(.Object@mD), format("%Y%m%d")) - as.Date(as.character(.Object@cD), format("%Y%m%d")))/365
            return(.Object)
          })