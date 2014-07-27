## This function should do two things: - return the inverse of a matrix 
## and check and see if the inverse of a particular matrix already exists, 
## and if it doesn't, compute it

## makeCacheMatrix() creates a cachmatrix structure ie, defines the structure 
## of the class and the methods one can use within the next function

makeCacheMatrix <- function(x = matrix()) {
    mat <- NULL
    #creates an empty vector 
    set <- function(y) {
      x <<- y
    #sets global vector x 
      mat <<- NULL
    #mat (variable which we are going to put a matrix set to NLL in global 
    #environment)
    }
    
    get <- function() x
    setsolve <- function() mat <<- solve(x)
    getInverse <- function() mat
    
    list(set = set, get = get,
         setsolve = setsolve,
         getInverse = getInverse)
    #returns a list of numbers that can be accessed by the $ operater in the 
    #next function
  }


## this function requires the previous function
## it will check and see if x already exists, if it doesn't it computes 
##the value of x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mat <- x$getInverse()
  if(!is.null(mat)) {
    message("getting cached data")
    return(mat)
  }
  data <- x$get()
  mat <- solve(data, ...)
  x$setsolve(mat)
  mat
}
