# The following two functions cache the inverse of a matrix

# makeCacheMatrix: This function creates a special "matrix" object that can 
# cache its inverse.
# Creates an object that has 4 functions: set(), get(), setinv() and getinv()

# OBS: Requires that the matrix supplied is always invertible.

makeCacheMatrix <- function(x = matrix()) {  # Initialize object x as the function argument, an empty matrix
  m <- NULL                                  # Initialize m as an object within the environment to be used later
  set <- function(y) {                       # Define setter to assign x and m values in the parent environment
    x <<- y                                  # Assign the input argument of set() function to the x object 
    m <<- NULL                               # Assign NULL to the m object
  }
  get <- function() x                        # Define getter for the matrix x
  setinv <- function(solve) m <<- solve      # Define setter to solve inverse matrix m
  getinv <- function() m                     # Define getter for the inverse matrix m
  list(set = set,                            # Name set() function defined above as 'set'
       get = get,                            # Name get() function defined above as 'get'
       setinv = setinv,                      # Name function setinv() defined above as 'setinv'
       getinv = getinv)                      # Name function getinv() defined above as 'getinv'
                                             # Assign all functions as elements within a list and return it to the parent environment
}


# cacheSolve: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been 
# calculated (and the matrix has not changed), then cacheSolve should 
# retrieve the inverse from the cache.

# OBS: Requires an input argument of type makeCacheMatrix()

cacheSolve <- function(x, ...) {             # Define input argument x, a makeCacheMatrix() type of object
  m <- x$getinv()                            # Retrieve the inverse matrix of the object x passed in as the argument
  if(!is.null(m)) {                          # Check if result m is NULL 
    message("getting cached data")
    return(m)                                # If result is not NULL, return the valid cached inverse matrix m to the parent environment
  }
                                             # If !is.null(m) is FALSE, then
  data <- x$get()                            # Use get() function to get the matrix from the input object x, then
  m <- solve(data, ...)                      # Calculate its inverse matrix 
  x$setinv(m)                                # Use setinv() function on the input object to set the inverse matrix in the input object
  m                                          # Return the value of the inverse matrix to the parent environment by printing the inverse matrix object
}


