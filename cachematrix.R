
## The purpose of the following two functions is to avoid unnecessary calls
## to the computationally expensive 'solve' function that returns the inverse
## of a matrix. Once the inverse of a matrix is computed by the 'cacheSolve' 
## function, it is stored as a state variable in an object created by the 
## 'makeCacheMatrix' function. This state variable is checked by 'cacheSolve'
## before making a call to 'solve', to avoid calling 'solve' more than once for
## the same matrix.
## Usage example:
##   m <- makeCacheMatrix(matrix(c(1,2,3,4), 2, 2))
##   inverse.m <- cacheSolve(m)


## Creates a matrix object that can hold its inverse in the state
##   variable 'inv'.
## Object methods: getting and setting the matrix and its inverse.
## Assumption: x assumed to be a (square and) invertible matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  #holds the inverse of x, initialization to NULL important
    set <- function(y) {
        x   <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Argument 'x' is a matrix object created by function 'makeCacheMatrix'
##   that may also hold its inverse.
## This function returns the inverse of the matrix in x:
##   if the inverse already exists in x, it is returned directly,
##   else it is computed and stored in x before it is returned.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()  #x may contain the inverse
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()  #x does not contain the inverse, compute it
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
