## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##makeCacheMatrix creates a special "vector", which is a list containing a function to 
## 1. setting the value of the vector
## 2. getting the value of the vector
## 3. setting the value of the mean
## 4. getting the value of the mean

makeCacheMatrix <- function(x = matrix(sample(1:8), 2, 4)) {
  inv <- NULL                                 ##initializing inverse as NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x                         ##function to get matrix x
  setinv <- function(inverse) inv <<- inverse  
  getinv <- function() inv                    ##function to get inverse of the matrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

## cacheSolve calculate the mean of the special "vector" created above. First it checks
## to see if the mean has already been calculated. If it has it gets the mean from the
## cache and skips the computation. Or, it calculates the mean of the 
## data and sets the value of the mean in the cache via the setsolve function. 

cacheSolve <- function(x, ...) {                  ##gets cache data
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data!")
    return(inv)                                   ##returns inverse value
  }
  data <- x$get()
  inv <- solve(data, ...)                         ##calculates inverse value
  x$setinv(inv)
  inv                                             ##returns a matrix that is the inverse of 'x'
}