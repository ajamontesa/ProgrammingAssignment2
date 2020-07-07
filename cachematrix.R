## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}


## Test on some invertible matrices

m1 <- matrix(c(1, 0, 0, 0, 1, -4, 0, 0, 1), nrow = 3, ncol = 3)
cm1 <- makeCacheMatrix(m1)
cacheSolve(cm1)
solve(m1)

m2 <- matrix(c(1, 1, 1, 1, 0, 1, 2, 3, 0, 0, 1, 3, 0, 0, 0, 1),
             nrow = 4, ncol = 4)
cm2 <- makeCacheMatrix(m2)
cacheSolve(cm2)
solve(m2)

m3 <- matrix(c(3, 4, 0, 0, 2, 3, 0, 0, 0, 0, 6, 7, 0, 0, 5, 6),
             nrow = 4, ncol = 4)
cm3 <- makeCacheMatrix(m3)
cacheSolve(cm3)
solve(m3)
