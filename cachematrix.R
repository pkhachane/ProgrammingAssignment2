## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##The first function, makeVector creates a special "vector", which is really a list containing a function to

# 1) set the value of the vector
# 2) get the value of the vector
# 3) set the value of the mean
# 4) get the value of the mean

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(a = matrix()) {
    # initialize the inverse matrix value
    mat_inv <- NULL
    
    # set the value of the matrix
    s_value <- function(b) {
        a <<- b
        mat_inv <<- NULL
    }
    
    # get the value of the matrix
    g_value <- function() a
    
    # set the value of the inverse
    set_inverse <- function(inv_input) mat_inv <<- inv_input
    # get the value of the inverse
    get_inverse <- function() mat_inv
    
    # return a list of all the above functions
    list(set = s_value, get = g_value,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
    
}


## Write a short comment describing this function

cacheSolve <- function(a, ...) {
    # check if the inverse is already cached,
    # if so, we get the inverse from the cache directly
    ginv <- a$get_inverse()
    if(!is.null(ginv)) {
        message("getting cached inverse")
        return(ginv)
    }
    # else, we first get the matrix
    matrixd <- a$get()
    # and calculate the inverse
    ginv <- solve(matrixd, ...)
    # next, cache the inverse of the matrix
    a$set_inverse(ginv)
    # and finally, return the result
    ginv
}

