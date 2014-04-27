## Since matrix inversion is usually a costly computation
## it is beneficial to cache the inverse of a matrix  
## and take it out directly when needed, 
## rather than to compute it repeatedly.

## This is a pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inver <<- inverse
        getInverse <- function() inver
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed)
## the function would retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
        inver <- x$getInverse()
        ## check if the inverse has already been calculated
        if(!is.null(inver)) {
                message("getting cached data")
                return(inver)
        }
        data <- x$get()
        ## calculated the inverse
        inver <- solve(data)
        x$setInverse(inver)
        inver
}
