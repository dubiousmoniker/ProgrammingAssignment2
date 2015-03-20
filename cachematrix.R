
## cachematrix.R can create an inverse matrix for a given matrix using a list
## of anonymous functions that set and get values from a cache. When returning 
## an inverse, the function first checks cache to see if that inverse matrix
## already exists and if so returns that value instead of a calculated inverse.
## 

## makeCacheMatrix creates a list of functions to be used in cacheSolve
#     * set:    sets a matrix to cache
#     * get:    returns the cached matrix
#     * setinv: sets the inversed matrix to cache using solve()
#     * getinv: returns the cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL   
  
  ## sets a matrix to cache, clears cached inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## return cached matrix
  get <- function() x
  
  ## sets inversed matrix in a cache 
  setinv <- function(solve) inv <<- solve
  
  ## returns the cached inverse matrix 
  getinv <- function() inv
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve checks for a cached inverse matrix and if it finds one, returns
## the cached value and a message to that effect. If not, it uses solve() to
## create an inverse matrix, saves it to cache and returns it.

cacheSolve <- function(x, ...) {
  # recalls cached inverse matrix
  inv <- x$getinv()
  
  # checks if recalled cache inverse matrix is null and if not returns from 
  # cache and lets user know that no new solve() calculation was performed
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # gets input matrix, saves inversed matrix to cache and returns it
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}