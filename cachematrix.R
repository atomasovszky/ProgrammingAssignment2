## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  
  get <- function()
  {
    return(x)
  }
  
  setInverse <- function(inverse)
  {
    inv <<- inverse
  }
  
  getInverse <- function()
  {
    return(inv)
  }
  
  return(
    list(
      get = get,
      set = set,
      getInverse = getInverse,
      setInverse = setInverse
    )
  )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        
        if(!is.null(inv))
        {
          message("Getting the inverse of matrix from cache")
          return(inv)
        }
        else
        {
          data <- x$get()
          inv <- solve(data)
          x$setInverse(inv)
          return(inv)
        }
}
