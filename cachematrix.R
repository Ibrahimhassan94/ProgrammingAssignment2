## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Matrix inversion is usually a costly computation and there 
# may be some benefit to caching the inverse of a matrix rather 
# than compute it repeatedly. The following two functions 
# are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) { 
     ## define the argument with default mode of "matrix"
     inv <- NULL        ## initialize inv as NULL; will hold value of matrix inverse 
     set <- function(y) {        ## define the set function to assign new 
          x <<- y                        ## value of matrix in parent environment
          inv <<- NULL                  ## if there is a new matrix, reset inv to NULL
     } 
     get <- function() x ## define the get fucntion-returns value of the matrix argument
     
     setinverse <- function(inverse) 
          inv <<- inverse  ## assigns value of inv in parent environment
     getinverse <- function() inv   ## gets the value of inv where called
     list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## you need this in order to refer 
     ## to the functions with the $ operator

}


## Write a short comment describing this function

# The following function returns the inverse of the matrix. 
# It first checks if the inverse has already been computed. 
# If so, it gets the result and skips the computation. 
# If not, it computes the inverse, sets the value in the 
# cache via  setinverse function.

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     inv <- x$getinverse()
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     data <- x$get()
     inv <- solve(data, ...)
     x$setinverse(inv)
     inv
}
