makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {                      #sets the value of the matrix. 
          x <<- y
          inv <<- NULL 
     }
     get <- function() x
     setInverse <- function(inverse) {
          inv <<- inverse     }
     
     getInverse <- function() inv
     
     #This returns the list of methods
     list(set = set,
          get=get, 
          setInverse = setInverse, 
          getInverse=getInverse)
}

cacheSolve <- function(x, ... ) { 
     inv <- x$getInverse()
     if(!is.null(inv)) {      #if the matrix has been used before it will return (true)
          message("getting cache matrix")
          return(inv)         
     }
     data <- x$get()     #otherwise, will use the solve method to invert the matrix
     inv <- solve(data, ...)
     x$setInverse(inv)
     
     
     inv  #return the inverse of the matrix
     
}


