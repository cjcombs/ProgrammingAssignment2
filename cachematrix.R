
## The following two functions allow user to calculate and cache the inverse
## of an input matrix and cache the result


## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = numeric()) {
        # ARGUMENTS
        #       'x' is a matrix, the inverse of which is to be calcualted and
        #               stored in cache m
        #
        # EXAMPLES
        #       'x$get()' returns original input object
        #       'x$getinverse()' returns cached inverse of object (default NULL)
        
        m <- NULL  # Creates empty cache to later store inverse matrix
        
        set <- function(y) {  # Enables manual replacement in global environment
                x <<- y
                m <<- NULL
        }
        
        get <- function() x  # Returns input matrix
        
        setinverse <- function(inverse) m <<- inverse  # Replace in glob env
        
        getinverse <- function() m  # Returns inverse from cache (default NULL)
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## Computes and caches inverse of the matrix returned by makeCacheMatrix
## If inverse has been calculated and matrix has not changed, retrieves
##      the inverse from cache
cacheSolve <- function(x, ...) {
        # ARGUMENTS
        #       'x' is a matrix, the inverse of which is to be calculated
        #       '...' allows additional arguments to be passed to the built-in
        #               R function, solve
        #
        # EXAMPLES
        #       'cacheSolve(x)' returns inverse of x
        #       'x$setinverse(m)' replaces cache value with argument value
        #       
        # Note on solve:
        #       solve(a, b, ...)' solves the equation a %*% x = b for x; if b
        #       is unspecified, 'solve(a, ...)' calculates the inverse of a
        
        m <- x$getinverse()  # Sets cache value to makeCacheMatrix cache value
        
        if(!is.null(m)) {  # If the cache in nonempty, retrieves from cache
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()  # If inverse of x is not cached, calculates and caches
        m <- solve(data, ...)
        x$setinverse(m)
        m
        
}
