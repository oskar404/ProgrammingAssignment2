## Very simple test for cachematrix.R functions

## Write a short comment describing this function

library("assertr")

test_cachematrix <- function() {
    print("Test1: Test with 3x3 identity matrix")
    m0 <- matrix(c(1,0,0, 0,1,0, 0,0,1) ,nrow = 3, ncol = 3)
    obj <- makeCacheMatrix(m0)
    verify(obj, obj$get() == m0)
    verify(obj, is.null(obj$get_cache()))
    val1 <- cacheSolve(obj)
    verify(obj, !is.null(obj$get_cache()))
    verify(obj, val1 == m0)
    val2 <- cacheSolve(obj)
    verify(obj, !is.null(obj$get_cache()))
    verify(obj, val2 == m0)
    verify(obj, val2 == val1)

    print("Test2: Test with 2x2 matrix and reuse object")
    m1 <- matrix(1:4 ,nrow = 2, ncol = 2)
    obj$set(m1)
    verify(obj, obj$get() == m1)
    verify(obj, is.null(obj$get_cache()))
    val3 <- cacheSolve(obj)
    verify(obj, !is.null(obj$get_cache()))
    verify(obj, val3 != m1)

    print("Tests: Done")
}
