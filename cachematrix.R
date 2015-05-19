## makeCacheMatrix caches a matrix and creates a cache for its inverse. It 
## returns a list with functions to get and set the matrix and its inverse.
## cacheSolve uses the matrix created in makeCacheMatrix to solve for the
## inverse and cache it. It will return the cache if the matrix is unchanged.

## makeCacheMatrix takes a matrix and caches it. It also creates a cache for the
## inverse of the matrix. 
## It returns a list consisting of functions (get, set, getinverse, setinverse) 
## to get and set the matrix and to get and set the inverse.
makeCacheMatrix <- function(x = matrix()) {
    # inv stores the inverse, initialized to NULL
    inv <- NULL
    # set the matrix and (re-)initialize the inverse to NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    # get the matrix previously provided (defaults to an empty matrix)
    get <- function() x
    # set the inverse to the user provided one
    setinverse <- function(inverse) inv <<- inverse
    # get the inverse from the user provided inverse
    getinverse <- function() inv
    # return the functions to access the inverse and matrix.
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve takes matrix created by makeCacheMatrix to find the inverse of the
## provided matrix. The inverse is cached and returned from the cache if it has
## not changed.
## The matrix is assumed to be invertible.
## Additional arguments after the matrix to invert are passed to the solve
## function.
cacheSolve <- function(x, ...) {
    # Check if the inverse exists. If so, return it.
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    # If the inverse doesn't exist, get the data and solve for the inverse
    data <- x$get()
    inv <- solve(data, ...)
    # Store the solved inverse in the cache.
    x$setinverse(inv)
    # Return the inverse.
    inv
}
