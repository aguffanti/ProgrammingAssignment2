## These functions create a cached version of the inverse of a matrix
## Input is an invertible (non-singular) matrix, output is the inverse

## Creates all tha appropriate objects, getters and setters methods

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        set_inv_matrix <- function(inv_matrix) m <<- inv_matrix
        get_inv_matrix <- function() m
        list(set = set, get = get,
             set_inv_matrix = set_inv_matrix,
             get_inv_matrix = get_inv_matrix)
}

## Computes or caches the inverse of the matrix

cacheSolve <- function(x, ...) {
        m <- x$get_inv_matrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$set_inv_matrix(m)
        m
}