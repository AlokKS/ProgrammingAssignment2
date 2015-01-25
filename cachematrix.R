## A function which wrapps another functions to create and cache matrix inverse for reuse

## Create wrapper for cache handling functions
makeCacheMatrix <- function(x = matrix()) {
    mx <- NULL
    
    #Initialize variable and cache
    setContext <- function(y){
      x <<- y
      mx <<- NULL
    }
    
    #Get context
    getContext <- function(){
      x
    }
    
    #Set matrix cache
    setMatrixInv <- function(mi){
      mx <<- mi
    }
    
    #Get Inversed matrix from buffer
    getMatrixInv <- function(x){
      mx
    }
    
    list( setContext = setContext, 
          getContext = getContext,
          setMatrixInv = setMatrixInv,
          getMatrixInv = getMatrixInv)
}


## Check availability of inversed matrix. In case it is cached, return value from cache otherwise perform matrix inversion, 
# update cache and return result

cacheSolve <- function(x, ...) {
    ## check cache
    mx <- x$getMatrixInv()
    if(!is.null(mx)){
      message("getting cached data")
      return(mx)
    }
    
    # cache is empty, hence calculate inversion and update cache as well
    data <- x$getContext()
    mx <- solve(data, ...)
    x$setMatrixInv(mx)
    return(mx)
}
