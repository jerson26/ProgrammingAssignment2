## These functions help in faster computation of matrices using cache

## this is the function for the first part, makecachematrix

makeCacheMatrix <- function(x = matrix()) {
    invers <- NULL

    pinvers <- function(solvmat) invers <<- solvmat
    cinvers <- function() invers
    
        
    cache <- function() x
    p <- function(y){
        x <<- y
        invers <<- NULL
    }
    
    list(p = p, cache = cache, pinvers = pinvers, cinvers = cinvers)
}

## this is a function for the second part cacheSolve

cacheSolve <- function(x, ...) {
    invers <- x$cinvers()
    if(!is.null(invers)){
        message("the data is now being cached")
        return(invers)
    }
    
    final <- x$cache()
    invers <- solve(final)
    x$pinvers(invers)
    invers
}
