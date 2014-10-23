## Create object to store and get matrix data and inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {

  #initialize inverse variable
  inverseOfx <- NULL

  # Declare set methods for both variables: 
  # matrix and inverse of that matrix 
  set <- function(y) {
    # Set variables if supplied matrix "y" differs from previously stored matrix "x"
    if (!identical(x,y) ) {
      message("setting variables")
      x <<- y
      inverseOfx <<- NULL
    }      
  }
  setInverse <- function(inverse) inverseOfx <<- inverse

  # Declare get methods for both variables: 
  # matrix and inverse of that matrix 
  get <- function() x
  getInverse <- function() inverseOfx

  # Create caller variables for methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Get inverse of matrix stored in object x of type "makeCacheMatrix"
## If inverse of matrix isn't stored in x
## claculate inverse, save it in object x, return inverse

cacheSolve <- function(x, ...) {

  # Get inverse stored in object x
  inverseOfx <- x$getInverse()

  # If inverse of matrix is stored in x return it
  if(!is.null(inverseOfx)) {
    message("getting cached data")
    return(inverseOfx)
  }

  # If inverse of matrix isn't stored in x
  # claculate inverse, save it in object x, return inverse
  data <- x$get()
  inverseOfx <- solve(data,...)
  x$setInverse(inverseOfx)

  inverseOfx
  
}
