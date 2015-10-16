## The functions below create a special "matrix" that can be inveted using solve,
## but will cache the result if inverted many times.

## Creates a special "matrix" that can be inverted and chaches the inverted matrix
makeCacheMatrix <- function(x = matrix()) {
        storedInverse <- NULL
	getMatrix = function(){
                x
        }
	setMatrix = function(newMatrix){
                ##invalidate the cache
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
