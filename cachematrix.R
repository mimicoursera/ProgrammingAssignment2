## Matrix inversion is usually a costly computation and their may be some benefit to caching the 
## inverse of a matrix rather than compute it repeatedly. This code includes 2 functions that cache 
## the inverse of a matrix.

## To use this code (e.g., try your_matrix <- matrix(c(1,-1,1,2),2,2)):
## 1- Assign makeCacheMatrix(your_matrix) to a new name, e.g.: out<-makeCacheMatrix(your_matrix)
## 2- Execute cacheSolve on that output: cacheSolve(out) (the second time you run this, you'll get 
## a message that cached data is being used)

## makeCacheMatrix is a function that creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsol <- function(solve) s <<- solve
    getsol <- function() s
    list(set = set, get = get,
         setsol = setsol,
         getsol = getsol)
}

## cacheSolve is a function that computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    s <- x$getsol()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsol(s)
    s
}
