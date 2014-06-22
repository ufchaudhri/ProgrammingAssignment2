## comment made by Umar Chaudhri 06/22/2014
## creates a special "vector", which is a list containing several functions


makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
	set <- function(y) {
		x<<- y
		m <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## comment made by Umar Chaudhri 06/22/2014
## function returns the inverse of a matrix. 
## Before calculating the inverse it first checks if the inverse in in cache
## if it exists then the cache inverse is returned

cachesolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

## y = cbind(c(1,2), c(2,1))
## n = makeCacheMatrix(y)
##cachesolve(n)
##cachesolve(n)
