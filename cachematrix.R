## These functions help avoid needless recalculations of matrix inverses by saving the inverse (when calculated) for the first time, into the obejct itself.
## For this purpose, it is clear that a simple matrix object will not suffice as it will not be able to store the calculated inverse. Instead a list of functions is created - one of which (get()) stores the matrix itself.

## makeCacheMatrix function takes a matrix and returns a list whose elements are functions for the storage and retrieval of the matrix data and inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## cacheSolve function takes a makeCacheMatrix object and returns the inverse of the matrix stored in the object
## When computed for the first time, the inverse is set in the makeCacheMatrix object itself using the setinverse function.

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
    ## Return a matrix that is the inverse of 'x'
}


## Testing:
b <- makeCacheMatrix(matrix(c(94,7,65,32), 2, 2)) # As recommended, only invertible matrices are to be used
b$get()
b$getinverse()
cacheSolve(b)
b$getinverse()
cacheSolve(b) # This time, the cached inverse is returned instead.