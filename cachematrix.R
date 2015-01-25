## These functions store a matrix to the cache, store its inverse to the cache, 
## and then return the inverse of either the stored matrix or stored inverse

## FUNCTION 1: makeCacheMatrix

## This function stores the matrix and its inverse to the cache. It creates a list 
## containing for functions, which operate as follows:

# 1.  $set caches the matrix
# 2.  $get retrieves the matrix from the cache
# 3.  $setinverse calculates the inverse of the matrix and then caches it
# 4.  $getinverse retrieves the inverse from the cache

makeCacheMatrix <- function(x = matrix()){
  
  # Clear any previously cached inverse  
  
  inv <- NULL
  
  # Create function to cache matrix x in the parent environment and clear cached inverses
  
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  # Create function to get x from the cache
  
  get <- function() x
  
  # Create function to take the inverse of x and store it in the object inv
  
  # Inverse can be set by inputting a matrix directly, or by calling a cached matrix using
  # matrix$setinverse(matrix$get()), where the "matrix" created by the makeCacheMatrix 
  # function has been stored in the object named matrix.
  
  setinverse <- function(x){
    inv <<- solve(x)
  }
  
  # Create function to get the inverse of x from the cache
  
  getinverse <- function() inv
  
  # Output functions created as a list
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}


#  FUNCTION 2: cacheSolve

#  This function returns a matrix that is an inverse of y (the matrix that was input)
#  If the inverse was cached with the makeCacheMatrix function, it will pull the inverse 
#  from the cache
#
#  If the matrix has been cached but the inverse has not, it will compute the inverse and 
#  return it.
#
#  This function will only work when the makeCacheMatrix function is run first. 
#  If the makeCacheMatrix has not been run, it is more efficient to take the inverse of
#  matrix x using solve(x)


cacheSolve <- function(y, ...){
  
  #Take matrix inverse from cache
  inverse <- y$getinverse()
  
  # Test whether inverse was available in cache
  
  if(!is.null(inverse))
    
    # If inverse exists in cache, report the message "Using data from cache"
    # and return the cached inverse
    
  {
    message("Using data from cache")
    return(inverse)
  }
  
  # If inverse is not cached, calculate inverse of cached matrix stored by 
  # makeCacheMatrix $set function and return the calculated inverse
  
  newmatrix <- y$get()
  inv <- solve(newmatrix)
  return(inv)
  
}
