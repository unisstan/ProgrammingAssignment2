## creates a matrix object that can cache its inverse
## there are four functions: setting the matrix, getting the matrix
## setting the inverse of the matrix, and getting the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setMatrix <- function(inverse = matrix()) m <<- inverse
	getMatrix <- function() m
	list(set = set, get = get,
             setMatrix = setMatrix,
             getMatrix = getMatrix)
}


## if the inverse has not been calculated, the function should
## compute the inverse of the matrix
## if the inverse has been calculated, the function should retrieve the
## inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getMatrix()
      if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data,...)
	x$setMatrix(m)
	m

}
