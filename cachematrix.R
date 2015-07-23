## makeCacheMatrix() and cacheSolve() are used together
## create a cache for the results of solve() so that it
## only needs to be calculated once.  An example of how they're
## used:
## > source("cachematrix.R")
## > mcm <- makeCacheMatrix(matrix(1:4, 2, 2))
## > cacheSolve(mcm)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## writeCacheMatrix provides a list of setters and getters
## for both the originally inputted matrix and the solved 
## matrix.  Note that using '<<-' instead of simply '<-'
## allows the 'x' and 's' variables in the top level of
## the function to be set from within the nested functions

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## cacheSolve() takes a makeCacheMatrix and determines if a
## cache already exists, in which case it returns it.  If not
## itexecutes solve on its data. It then caches it in its 
## makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
