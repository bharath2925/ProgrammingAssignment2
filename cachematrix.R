## The below pair of functions is used to cache the inverse of a matrix and use the cached value when the inverse matrix 
## computation is required.

## The first function creates a special matrix object used to store the inverse matrix values.

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<- function(y) {
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setInv<-function() inv <<-solve(x)
  getInv<-function() inv
  
  list(set = set, get = get, setInv = setInv, getInv = getInv)
  
}


## The second function Computes the inverse of the special "matrix" returned by makeCacheMatrix function. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache. Else the inverse is computed using the above function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv<- x$getInv()
  if (!is.null(inv)) {
    message("getting cached matrix")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data, ...)
  x$setInv(inv)
  inv
}
