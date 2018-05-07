## Functions Make Cache and Cache solve
### Functions will search to see if there's already a computed inverse 
### matrix in the cache, if not they will solve the Matrix Computation
### Makes a list with the cached matrix, the cached solve and the 
### Setters and Getters for both

makeCacheMatrix <- function(x = matrix()) { 
        Inv <- NULL
        set <- function(y) {
                x <<- y
                Inv <<- NULL
        }
        get <- function() x
        setSolve <- function(Solve) Inv <<- Solve
        getSolve <- function() Inv
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


### Looks for a cached Inv that corresponds to x, 
### if it's null then it does the computation and stores Inv in the cache
cacheSolve <- function(x, ...) {
        Inv <- x$getSolve()
        if(!is.null(Inv)) {
                message("getting cached data")
                return(Inv)
        }
        data <- x$get()
        Inv <- solve(data, ...)
        x$setSolve(Inv)
        Inv
}