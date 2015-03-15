## Implemention of a matrix that caches the inverse of the matrix when it is
## calculated, and clears the cache when the matrix changes

## makeCacheMatrix creates a matrix implemented as a list of functions for setting and getting
## the matrix itself and its inverse.  The inverse is NULL initially, and
## is reset to NULL whenever the matrix is 'set'
makeCacheMatrix <- function(x = matrix()) {
    i<-NULL  # new matrix, inverse not calculated yet
    set<-function(aMatrix) {
        ## Note that the x and i we want are in the environment of 'makeCacheMatrix', 
        ## not 'set', therefore we use <<-
        x<<-aMatrix  
        i<<-NULL ## matrix changed, clear inverse
    }
    get<-function() x
    setInverse <- function(inverse) i<<-inverse
    getInverse <- function() i
    list(set=set, get=get,   ## return functions in a list
         setInverse=setInverse,
         getInverse=getInverse)
}


## cacheSolve returns the inverse of a matrix created using makeCacheMatrix,
## taking it from the cache if it is there, and otherwise calculating it
## and cacheing it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i<-x$getInverse()
    if(!is.null(i)) {  ## if the inverse is in the cache, return it
        message("getting cached inverse")
        return(i)
    }
    m<-x$get()  ## Otherwise, get the matrix and 
    i<-solve(m) ## find its inverse,
    x$setInverse(i)  ## cache the result
    i ## and return it
}
