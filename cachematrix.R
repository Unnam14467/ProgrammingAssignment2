

## Write a short comment describing this function

## The function does the preparation for caching and stuff
## Actual computing happens in the next function
makeCacheMatrix <- function(x = matrix()) 
{
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## Function to get data
  get <- function() x
  
  ## We are caching matrix
  setinverse <- function(solve) m <<- solve
  
  ## Simply return the computed inverse of matrix
  getinverse <- function() m
  
  ## What the function returns : basically a list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Real inverse computation takes place here


cacheSolve <- function(x, ...) 
{
  ## We get the matrix
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  
  ## Executes if we don't have cache
  data <- x$get()
  ## We compute the inverse only first time
  m <- solve(data, ...)
  ## Now we cache inverse so that we don't need to solve the inverse again
  x$setinverse(m)
  m        ## Return a matrix that is the inverse of 'x'
}
