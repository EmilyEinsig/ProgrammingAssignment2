## Put comments here that give an overall description of what your
## functions do
# Functions allow the user to cache the inverse of a matrix to save time
# on potentially time costly computations 

## Write a short comment describing this function
# Function creates a special "matrix" object than can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

	inv <- NULL

	set <- function(y) {
		x <<- y
		inv <<- NULL
	}

	get <- function() x

	setinv <- function(solve) inv <<- solve

	getinv <- function() inv

	list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## Write a short comment describing this function
# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve 
# the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	inv <- x$getinv()

	if(!is.null(inv)){
		message("Getting cached data")
		return(inv)
	}
	
	message("Calculating")
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv

}
