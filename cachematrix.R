## Put comments here that give an overall description of what your
## functions do

# 2 fns that cache the inverse of a matrix

## Write a short comment describing this function

# creates a matrix with a cache for its inverse

makeCacheMatrix <- function(x = matrix()) 
{
  # Initialize the inverse property
  inv <- NULL
  
  # set the matrix
  set <- function(y) 
  {
    x <<- y
    inv <<- NULL
  }
  
  # get the matrix
  get <- function() x
  
  # set the cached inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  
  # get the cached inverse of the matrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## Write a short comment describing this function

# takes the matrix object created above as its first argument and 
# uses the 'getInverse' fn to check if the inverse is already cached

cacheSolve <- function(x, ...) 
{
## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  
  # if the inverse is already set, return that otherwise;
  if (!is.null(inv)) 
  {
    message("getting cached data.")
    return(inv)
  }
  # get the matrix 
  data <- x$get()
  
  # use the 'solve' fn to return a matrix 
  inv <- solve(data)
  
  # set the inverse to the object
  x$setInverse(inv)
  
  # return
  inv
}
