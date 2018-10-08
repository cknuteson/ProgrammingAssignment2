## Put comments here that give an overall description of what your
## functions do

# `makeCacheMatrix`: This function creates a special "matrix" object
#     that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(solve) i <<- solve
        getinv <- function() i
        list(set = set, get = get,
                setinv = setinv,
                getinv = getinv)
}

#  `cacheSolve`: This function computes the inverse of the special
#     "matrix" returned by `makeCacheMatrix` above. If the inverse has
#     already been calculated (and the matrix has not changed), then
#     `cacheSolve` should retrieve the inverse from the cache.

# Computing the inverse of a square matrix can be done with the `solve`
# function in R. For example, if `X` is a square invertible matrix, then
# `solve(X)` returns its inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
