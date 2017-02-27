## These functions create a matrix object that can cache its inverse, and then either returns the cached inverse 
## or solves for the inverse, if the inverse has not previously been caclulated.

## This function takes a matrix and creates an object with 4 functions: 
## 1. get - this function accesses the matrix
## 2. set - this function mutates the matrix
## 3. getinverse - this function access the inverse of the matrix
## 4. setinverse - this function mutates the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function (y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <-function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get= get,
	setinverse = setinverse,
	getinverse = getinverse)
}

## This function checks to see if the inverse of the matrix has already been computed.
## If it has, it returns the inverse, which has been cached. If the inverse of the matrix
## has not been computed,this function solves the inverse of the matrix and caches the inverse.

cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
}

