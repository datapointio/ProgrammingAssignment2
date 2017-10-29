## Here is the pair of functions that 
## helps to compute inversion of square matrix and
## helps to cache a result so it could be used further without repeated computing
## Example:
## mat <- makeCacheMatrix( matrix(c(1,2,3,4),2,2) )
## inv <- cacheSolve(mat)


# Return a list of functions

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  get <- function() x
  
  setmatrix <- function(inversion) m <<- inversion
  
  getmatrix <- function() m
  
  list(get = get, setmatrix = setmatrix, getmatrix = getmatrix)
  
}

# Return inverse matrix from cache or compute it if not cached

cacheSolve <- function(x, ...) {
  
  m <- x$getmatrix()
  
  if(!is.null(m)) {
    
    message("Getting cached data")
    return(m)
    
  }
  
  data <- x$get()
  
  if(det(data) == 0) {
  
    message("Matrix is not invertible")
     
  } else {
  
    m <- solve(data, ...)
    
    x$setmatrix(m)
    
    m
    
  }
  
}