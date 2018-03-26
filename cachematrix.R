##################################
## programming assignment 2: 
## caching the inverse of a matrix
## done by thomas klinger
##################################

# this function creates a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                 # init inv
    set <- function(y) {    
        x <<- y                 # assign input to x
        inv <<- NULL            # clear inv in parent
    }
    get <- function() x         # retrieve all from parent
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,  # create list and return
         setinverse = setinverse,
         getinverse = getinverse)
}


# this function requires an argument returned by makeCacheMatrix
# so that it can retrieve the inverse from the cached value
# from the environment of makeCacheMatrix

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()    # try to retrieve an inverse from x
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)          # cached inverse available
    }
    data <- x$get()
    inv <- solve(data, ...)  # calculate the inverse 
    x$setinverse(inv)        # and set it
    inv
}
