
## This program Cache Inverse of a Matrix to save  time-consuming inverse
#of matrix computations.

##The makeCacheMatrix fuction creates a special square "matrix" object that 
  #can cache its inverse, which is really a list containing a function to
  #1. set the value of the matrix
  #2. get the value of the matrix
  #3. set the value of the inverse of matrix
  #4. get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) 
{
    inv <- NULL
    setmatrix <- function(y) 
    {
      x <<- y
      inv <<- NULL
    }
    getmatrix <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinv = setinv,
       getinv = getinv)
}


##The cacheSolve function calculates the inverser of matrix of the square matrix
  #created with the above function.However, it first checks to see if the inverse
  #matrix  has already been calculated. If so, it gets the inverse of matrix from
  #the cache and skips the computation. Otherwise, it calculates the inverse of the 
  #matrix and sets the value of the matrix in the cache via the setmatrix function.

cacheSolve <- function(x, ...) 
{
  inv <- x$getinv()
  if(!is.null(inv)) 
  {
    message("getting cached data")
    return(inv)
  }
  data <- x$getmatrix()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
