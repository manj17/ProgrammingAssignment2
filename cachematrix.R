##This code represents my submission to the second assignment in Programming in R
##The functions below work to create or simply retreive (is available) the inverse of an invertible matrix

##  This function creates a special "matrix" that is able to cache it's inverse, as per the second assignment in Programming in R
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function (answer) inv <<- answer
        getInv <- function () inv
        list (set = set, get = get,  setInv = setInv, getInv = getInv)
}


## This function computes the inverse returned from makeCacheMatrix. It will retrieve the inverse if already available, as per second assignment in Programming in R
cacheSolve <- function(x, ...) {
        inv <- x$getInv()
        if(!is.null(inv)){
                message("Getting inverse of matrix...")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setInv(inv)
        inv
}
