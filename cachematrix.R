## makeCacheMatrix and cacheSolve allow a user to enter a matrix, find
## the inverse matrix, and cache/retrieve this inverse matrix

## the first part, makeCacheMatrix, gives us the tools to set and 
## get a matrix, and set and get a cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL

	## set function allows us to set a value for x; m is NULL until an
	## inverse matrix is cached
	set <- function(y) {
		x <<- y
		m <<- NULL
	}

	## get function retrieves the value for x
	get <- function() x

	## setinverse allows us to set a cached inverse matrix, "m"
	setinverse <- function(inverse) m <<- inverse

	## getinverse retrieves the cached inverse matrix
	getinverse <- function() m

	## list allows us to retrieve these functions from cacheSolve
	list(set = set, get = get,
	        setinverse = setinverse,
		getinverse = getinverse)
}


## cacheSolve returns the inverse matrix for x, first checking to see
## if an inverse has already been cached

cacheSolve <- function(x, ...) {
        ## retrieving value of m from makeCacheMatrix
	m <- x$getinverse()

	## if m is not null (an inverse has been set), the cached value is retrieved
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	
	## if there is no cached inverse, a new inverse is found
	data <- x$get()
	m <- solve(data, ...)

	## the new inverse is cached
	x$setinverse(m)

	## the new cached inverse is printed
	m
}
