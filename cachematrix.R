## Put comments here that give an overall description of what your
## functions do

# Load the MASS package to use the generalized inverse
library(MASS)

# makeCacheMatrix creates a list of functions to interact with a matrix and its inverse
# The main purpose of this code is to improve computational efficiency by avoiding 
# redundant calculations of the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
         inv <- NULL                 # Starting inverse as NULL to store the cached
                                     # inverse of the matrix
         set <- function(y) { 
         x <<- y                     # this function updates the matrix x with a new value y
         m <<- NULL
  }
   get <- function() x               # Function to get the current matrix x
   
   setinv <- function(inverse) inv <<- inverse
   
   getinv <- function() {           # This function calculates the inverse of the matrix x using ginv(x)
         inver <- ginv(x)
         inver %*% x           
   }
   list(set = set, get = get,      # The function returns a list containing the four functions: set, get, setinv, and getinv
       setinv = setinv,
       getinv = getinv)
}



# The function cacheSolve is used to get the cache data
# It computes the inverse of the matrix stored in makeCacheMatrix
cacheSolve <- function(x, ...) {
       inv <- x$getinv()         # checks if the inverse of the matrix is already cached in the object
       if(!is.null(inv)) {
        message("getting cached data")
        return(inv)             # If the cached inverse is not NULL, it returns the cached inverse
      }
       data <- x$get()
       inv <- solve(data,...)    # It computes the inverse of the matrix using the solve function
       x$setinv(inv)
       inv                       # Returns a matrix that is the inverse of x
  }
