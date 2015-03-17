## caching the inverse of a matrix
## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the inverse
#4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        cache <- NULL
        set <- function(y) {
                x <<- y
                cache <<- NULL
        }
        get <- function() x
        setMatrix <- function(inverse) cache <<- inverse
        getInverse <- function() cache
       
        ##return the functions to the working environment
        list(set = set, get = get,
             setMatrix = setMatrix,
             getInverse = getInverse)
}

## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then`cacheSolve` 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cache <- x$getInverse()
        if (!is.null(cache)) {
                message("getting cached data")
          
                return(cache)
        }
        matrix <- x$get()
        cache <- solve(matrix, ...)
        x$setMatrix(cache)
        return (cache)
        
}
