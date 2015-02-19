## Because matrix inversion is usually a costly computation, this function pair caches the inverse of a matrix, 
## rather than compute it repeatedly.  

## Example: 
## mat <- matrix(c(4,2,7,6), nrow=2, ncol=2)
## mat2 <- makeCacheMatrix(mat)
## cacheSolve(mat)

##      [,1] [,2]
## [1,]   -2  1.5
## [2,] -0.2  0.4

## The function below creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inv) inverse <<- inv
        getInverse <- function() inverse
        list (set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## The function below computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                return(inverse)        
        }
        data <- x$get()
        #message("solving the inverse for the first time")
        inv <- solve(data)
        x$setInverse(inv)
        inv
}
