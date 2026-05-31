## Put comments here that give an overall description of what your
## functions do

## This function, set a new matrix,get it, stores and retrieve the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        # Set a new matrix (resets the cached inverse)
        set <- function(y) {
                x   <<- y
                inv <<- NULL
        }
        
        # Get the matrix
        get <- function() x
        
        # Store the inverse in cache
        setinverse <- function(inverse) inv <<- inverse
        
        # Retrieve the cached inverse
        getinverse <- function() inv
        
        list(set        = set,
             get        = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function retrieves what's already cashed, or re-caches

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        
        # If already cached, return it directly
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # Otherwise compute, cache, and return
        data <- x$get()
        inv  <- solve(data, ...)
        x$setinverse(inv)
        inv
}
