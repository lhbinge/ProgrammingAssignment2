## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than computing it repeatedly 

##-------------------------------------------------------------------------------------
## makeCacheMatrix creates a special 'matrix' object that can cache its inverse
## makeCacheMatrix is a function that stores a list of functions

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## For example:
## a <- makeCacheMatrix(matrix(1:9, nrow=3, ncol=3))
## a$get()
## a$set(matrix(6:9, nrow=2, ncol=2))
## a$get()
## a <- makeCacheMatrix(matrix(1:6, nrow=2, ncol=3))
## a$setinverse(matrix(1:6, nrow=2, ncol=3))
## a$getinverse()

##------------------------------------------------------------------------------------
## cacheSolve computes the inverse of the 'matrix' returned by makeCacheMatrix above
## If the inverse has already been calculated, it retrieves the inverse from the cache

cacheSolve <- function(x, ...) {  
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

## For example:
## b <- c(-1, 0, -5, 3, -6, -3, -3, 5, 1)
## c <- matrix(b, nrow=3, ncol=3)
## a <- makeCacheMatrix(c)
## cacheSolve(a)
