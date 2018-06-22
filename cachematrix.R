#1.makeCacheMatrix: This function creates a special "matrix" 
#object that can cache its inverse.

#2. cacheSolve: This function computes the inverse of the special 
#"matrix" returned by makeCacheMatrix above. If the inverse has 
#already been calculated (and the matrix has not changed), then the 
#cachesolve should retrieve the inverse from the cache.


#Create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x=matrix()) {
  inv_matrix <- NULL
  #set the value of the matrix
  set <- function(y) {
    x <<- y
    inv_matrix <<- NULL 
  }
  #get the value of the matrix
  get <- function() x
  #set the value of the inverse of the matrix
  set_inv_matrix <- function(solve) inv_matrix <<- solve
  #get the value of the inverse of the matrix
  get_inv_matrix <- function() inv_matrix
  list(set = set, get = get, set_inv_matrix = set_inv_matrix, 
       get_inv_matrix = get_inv_matrix)
}



#create a function, cacheSolve, that calculates the inverse of the matrix or
#retrieves it from cache if the inverse has already been calculated
cacheSolve <- function(x, ...) {
  
  ## x is the output of makeCacheMatrix()
  ## return the inverse of the original matrix input to makeCacheMatrix()
  
  inv_matrix <- x$get_inv_matrix()
  
  # if the inverse has already been calculated
  if(!is.null(inv_matrix)) {
    
    # get it from the cache and skips the computation. 
    message("getting cashed data...")
    return(inv_matrix)
  }
  
  # otherwise, calculate the inverse 
  data <- x$get()
  inv_matrix <- solve(data, ...)
  
  # set the value of the inverse in the cache via the set_inv_matrix function.
  x$set_inv_matrix(inv_matrix)
  
  inv_matrix
  
}
