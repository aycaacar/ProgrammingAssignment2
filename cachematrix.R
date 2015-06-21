## Put comments here that give an overall description of what your
## functions do

## Creates a special data structure to hold a matrix and cache its inverse
## has methods : set, get, setinverse, getinverse and list
## inverse is the name of the internal variable storing cached value.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(m) inverse <<- m
        getinverse<- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## gets inverse of a matrix, checking if it was calculated and cached previously.
## returns the cached value if available.
## its input is an object created as a result of a call to makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        ## check if inverse was cached before, return cached value if so.
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        ## if no cached value, solve.
        matrix <- x$get()
        inverse <- solve(matrix)
        x$setinverse(inverse)
        inverse
}

