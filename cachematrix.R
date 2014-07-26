## Put comments here that give an overall description of what your
## functions do

## MakeCacheMatrix accepts the value of matrix based on the value of matrix inputted and stores the inverse in Cache
## as solved by Cachesolve 

makeCacheMatrix <- function(x = matrix(c(), nrow= 0, ncol = 0)) 
  ## Initialize the makecachematrix function and initalize as matrix as stored in Variable x
{ 
  inverseofmatrix <- NULL ## Initailize variable inverseofmatrix and pass it as null
  set <- function(y) { ## Initalize function set
    x <<- y  ## Assign x value in memory, post searching in enviornment
    inverseofmatrix <<- NULL ## Assign inverseofmatrix value as Null, in memory, post searching in enviornment
  }
  
  get <- function() x ##  Initialize get function
  
  setmatrixinv <- function(matinv) ## Initialize setmatrixinv as function and initailize variable matinv
  {
    inverseofmatrix <<- matinv ## Assign inverseofmatrix value in memory as stored in matinv
  }
  
  getmatrixinv <- function() ## Initialize getmatrixinv as function
  {
    inverseofmatrix ## Pass the value of inverseofmatrix
  }
  
  list(set = set, get = get, setmatrixinv = setmatrixinv, getmatrixinv = getmatrixinv)
  ## Create a list of Values
}


## cacheSolve solves the matrix and inputted in MakeCacheMatrix and find its inverse 
## if value is not already stored in the cache/memory.

cacheSolve <- function(x, ...) 
  ## Initialize the cacheSolve function and initalize as function with variable x which takes the input as list
  ## passed from makeCacheMatrix function above
{
  
  inverseofmatrix <- x$getmatrixinv() ## Fetch value of inverseofmatrix from environment
  
  if(!is.null(inverseofmatrix)) ## If inverseofmatrix value already exists in cache/memory 
    ##then return the stored value
  {
    message("getting cached data")
    return(inverseofmatrix)
  }
  
  data <- x$get() ##  If inverseofmatrix value doenst exist in cache then
  ##Get the input value of matrix as passed as a list in makeCacheMatrix function
  inverseofmatrix <- solve(data) ## Do the mathematical operation of inverse of matrix using the solve()function and
  ## store the value in inverseof matrix variable
  x$setmatrixinv(inverseofmatrix) ## Setvalue of inverseofmatrix in environment
  inverseofmatrix
}
