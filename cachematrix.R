## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
    i<-NULL  # new matrix, inverse not calculated yet
    set<-function(aMatrix) {
        x<<-aMatrix
        i<<-NULL # matrix changed, clear inverse
    }
    get<-function() x
    setInverse <- function(inverse) i<<-inverse
    getInverse <- function() i
    list(set=set, get=get, 
         setInverse=setInverse,
         getInverse=getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
