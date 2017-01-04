## The MakeCacheMatrix function creates a "matrix" object that can cache the inverse
## for an invertable matrix

MakeCacheMatrix <- function(x = matrix()) {   ## MakeCacheMatrix function
  
  mat_inverse <- NULL                         ## initializing the matrix-inverse variable
  set <- function(y) {
    x <<- y
    mat_inverse <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) mat_inverse <<- inverse
  getinv <- function() mat_inverse
  list(set = set, 
       get = get, 
       setinv = setinv, 
       getinv = getinv)
}

## The CacheSolve function checks if the inverse matrix already exixts. If it exists (and the" 
## matrix has not changed, it returns the inverse from cach. Otherwise it will be calculated

CacheSolve <- function(x, ...) {              ## CacheSolve function
                                              ## this function returns the inverse matrix of 'x'
  
  mat_inverse <- x$getinv()
  if(!is.null(mat_inverse)) {                 ## check whether mat_inverse exists
    message("getting cached result")
    return(mat_inverse)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(mat_inverse)
  mat_inverse
}
