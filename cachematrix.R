## These functions allow for an inverse of a matrix to be calculated 
## and stored in cache, so the next time it is only retrieved, not recalculated
## an example is:
##      makeCacheMatrix.object <- makeCacheMatrix(x) 
##      cacheSolve(makeCacheMatrix.object) #the first time this will run solve
##      cacheSolve(makeCacheMatrix.object) #the second time this will grab cache


## makeCacheMatrix is composed of three function, get(),setinv(), getinv()
makeCacheMatrix <- function(x = matrix()) {
                m <- NULL
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                get <- function() x
                setinv <- function(solve) m <<- solve
                getinv <- function() m
                list(set = set, get = get,
                     setinv = setinv,
                     getinv = getinv)
        
}


## cachesolve gets the cache value of the inverse of a matrix. 
## If not existent, it will calculate it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}

