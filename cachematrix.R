## Caches the inverse of a matrix rather than computing it
repeatedly

## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set < function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, 
             get = get,
             setinverse = setinverse
             getinverse = getinverse)
}

## This function computes the inverse of the special
## "matrix" 

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- inverse(data,...)
        x$setinverse(i)
        i
}