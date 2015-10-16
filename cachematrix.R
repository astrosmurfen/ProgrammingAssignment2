## The functions below create a special "matrix" that can be inverted using cacheSolve,
## cacheSolve will store the inverted matrix for faster repeated solves

## Creates a special "matrix" that can be inverted and 
## supply methods to access and cache the embedded matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
        storedInverse <- NULL
	getMatrix = function(){
                x
        }
	setMatrix = function(newMatrix){
                ##invalidate the cache since we have a new matrix
                storedInverse <<- NULL
                x <<- newMatrix
        }
        getCachedInverse = function(){
                storedInverse
        }
        setCachedInverse = function(inverse){
                storedInverse <<- inverse
        }
	list(getMatrix = getMatrix,
             setMatrix = setMatrix,
             getCachedInverse = getCachedInverse,
             setCachedInverse = setCachedInverse)
}


## Invert a "matrix" returned by makeCacheMatrix. Cache the result for performance 
## The return value is a normal matrix that is the inverse of the normal matrix used to initialize makeCacheMatrix. 
cacheSolve <- function(x, ...) {
	inverse = x$getCachedInverse()
        if (is.null(inverse)){
                matrix = x$getMatrix()
                inverse = solve(matrix,...)
                x$setCachedInverse(inverse)               
        }
        inverse
}
