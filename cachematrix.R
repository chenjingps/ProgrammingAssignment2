## This function calculates the inverse of a squre matrix.
## It uses caches to speed up the calculation.
##
## Note: the matrix has to be a square invertible matrix


## Here to create a list of function to:
#       - set the matrix
#       - get the matrix
#       - set the inverse
#       - get the inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function(){
                x
        }
        setinv <- function(inverse){
                inv <<- inverse
        }
        getinv <- function(){
                inv
        }
        list(set = set, get = get,
             setinv = setinv, 
             getinv = getinv)
}


## Here is to solve the inverse calculation,
# If cached, get the inverse directly from the cache,
# Otherwise, solve it and cache it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv();
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        theMatrix = x$get();
        inv <- solve(theMatrix,...)
        x$setinv(inv)
        inv
}