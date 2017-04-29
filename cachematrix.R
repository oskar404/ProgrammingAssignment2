## Coursera: R Programming
## Programming Assignment 2: Lexical Scoping

# Create object which has a cache
#
# The object has a simple get() and set() interface for manipulating the object
# value. The cache content is manipulated with get_cache() and set_cache().
makeCacheMatrix <- function(x = matrix()) {

    # Reset cache
    x_cache <- NULL

    # Set the value for the object and reset the cache
    set <- function(value) {
        x <<- value
        x_cache <<- NULL
    }

    # Return the object value
    get <- function() x

    # The cache interface functions set_cache() and get_cache()
    set_cache <- function(value) x_cache <<- value
    get_cache <- function() x_cache

    list(set = set, get = get,
         set_cache = set_cache,
         get_cache = get_cache)
}


# Returns the inverse of matrix x.
#
# The object x must be a cacheMatrix. This avoids recalculating the matrix
# inverse if it has been calculated already. The inverse is calculated with
# solve() and the extra arguments are passed to the solve() function.
cacheSolve <- function(x, ...) {
    # Check if the value is already calculated
    m <- x$get_cache()
    if(!is.null(m)) {
        return(m)
    }
    # Not found so calculate the inverse
    data <- x$get()
    m <- solve(data, ...)
    x$set_cache(m)
    m
}
