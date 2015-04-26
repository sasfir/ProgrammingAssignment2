
cachemean <- function(x, ...) {
}

##########
##  Functions to cache the inverse of a matrix.
##########

# The first function, makeCacheMatrix creates a special "matrix", which is  
# a list containing a function to
# set the value of the matrix
# get the value of the matrix
# calculate the inverse of the matrix
# get the inverse of the matrix
# Input should be an invertable matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(x_inv) inv <<- x_inv
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


# computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# Return the inverse if the cached value is not null. 
# Input should be the "matrix" returned by makeCacheMatrix, additional
# arguments can be supplied and will be sent to solve()


cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
