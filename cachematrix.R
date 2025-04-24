## Put comments here that give an overall description of what your
## functions do

# These functions provide an efficient way to calculate and retrieve the inverse
# of a matrix. They utilize a caching mechanism to store the matrix and its
# inverse, saving computation time by returning the stored inverse if it has
# been previously calculated.


## Write a short comment describing this function

# This function returns a list, that has: 1) function to get origional matrix 2)
# Function to assign value to variable "cache" through variable "inverse" 3)
# function to get inverse matrix stored in variable cache

makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  get_x <- function(){x}
  set_inv <- function(inverse){cache <<- inverse}
  get_inv <- function(){cache}
  list(get_x = get_x, set_inv = set_inv, get_inv = get_inv)
}


## Write a short comment describing this function

# This function takes object created by makeCacheMatrix(). first it checks, if
# inverse matrix is already stored in the list object. If available, it will
# return the stored inverse matrix directly, if not, will calculate inverse
# matrix, store it in the variable "cache" in the list object generated through
# makeCacheMatrix() using set_inv() and finally return the inverse matrix.

cacheSolve <- function(x, ...) {
  cache <- x$get_inv()
  if(!is.null(cache)){
    cat("Retrieving data from cache\n")
    return(cache)
  } else {
    data <- x$get_x()
    cat("Calculating the inverse\n")
    inverse <- solve(data,...)
    x$set_inv(inverse)
    return(inverse) ## Return a matrix that is the inverse of 'x'
  }
}
