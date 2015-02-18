# The first function, makeCacheMatrix creates a special "matrix", 
# which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  # initialize the value of the inverse
  inv <- NULL
  
  # set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # get the value of the matrix
  get <- function() x
  
  # set the value of the inverse
  set_inverse <- function(inv_input) inv <<- inv_input
  # get the value of the inverse
  get_inverse <- function() inv
  
  # List all the above functions
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
  
}

# The following function calculates the inverse of the special 
# "matrix" created with the above function. 
# It first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the 
# computation. Otherwise, it calculates the inverse of the 
# matrix and sets the value of the inverse in the cache via 
# the setinv function.

cacheSolve <- function(x, ...) {
  # check if the inverse is already calculated,
  # if so, we get the inverse from the calche and skips the computation 
  inv <- x$get_inverse()
  if(!is.null(inv)) {
    message
    return(inv)
  }
  # Otherwise, we get the matrix
  data <- x$get()
  #calculate the inverse
  inv <- solve(data, ...)
  #cache the inverse of the matrix
  x$set_inverse(inv)
  # finally, return the result
  inv
}
