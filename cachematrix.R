# makeCacheMatrix - input is a matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() { 
    x
  }
  
  setinv <- function(inv) {
    m <<- inv 
  }
  
  getinv <- function() {
    m
  }
  
  # Return a list of the functions to:
  # 1) set the matrix
  # 2) get the matrix
  # 3) set the inverse of the matrix
  # 4) get the inverse of the matrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


# cacheSolve - input is a special matrix created from the makeCacheMatrix function
cacheSolve <- function(x, ...) {

  # First, check if the inverse was already computed
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # If not, then get the matrix, solve to find the inverse, and save it for future use
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  
  ## Return a matrix that is the inverse of 'x'
  m
}


# Test this with the following example:
#a<-matrix(c(4, 3, 3, 2), nrow=2, ncol = 2)
#b<-matrix(c(1,0,1,2,4,0,3,5,6), nrow=3, ncol=3)

#mcmA <- makeCacheMatrix(a)
#mcmB <- makeCacheMatrix(b)

#cacheSolve(mcmA)
#cacheSolve(mcmA)
#cacheSolve(mcmA)

#cacheSolve(mcmB)
#cacheSolve(mcmB)
#cacheSolve(mcmB)
