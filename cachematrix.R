## Module to create CacheMatrix objects, and compute their inverse.

## Creates a CacheMatrix object, which encapsulates a matrix and a set
## of functions to compute its inverse, store it, and retrieve it.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(marg)
	{
		x <<- marg
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inva) inv <<- inva
	getinverse <- function() inv
	list(set = set,
		 get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)
}


## Takes a CacheMatrix object, and computes the inverse of the 
## matrix, or retrieves its value if it has already been computed.

cacheSolve <- function(x, ...) {
	inv = x$getinverse()
	if (!is.null(inv))
	{
		message("getting cached inverse")
		return(inv)		
	}
	inv = solve(x$get(), ...)
	x$setinverse(inv)
	inv
}
