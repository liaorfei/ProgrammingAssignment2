## The following code contains a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
## Its output is then used as an input for the cacheSolve function below.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL 
  }
  
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv()
  if(!is.null(inv)) {
    
    message("getting cached data")
    return(inv)
  }
  
  else {
    message("wait...calculating inverse of matrix")
    mymatrix <- x$get()
    inv <- solve(mymatrix, ...)
    x$setinv(inv)
    return(inv)
  }
}


## The following is a test to try if time is actually saved when retrieving cached results instead of calculating them again each time
## I am going to use the internal clock to measure the time difference, accessible through the Sys.time

time_test = function(mymatrix){
  # We need to save the output of the first function in order to be able to use it as an input for the second function
  firstfun = makeCacheMatrix(mymatrix)
  
  ## FIRST RUN: this time there is no cached result so the second function will actually perform the inversion
  
  # Save the starting time to calculate how long the inversion lasts in both cases
  time1 = Sys.time()
  
  # Run the second function that actually inverts the matrix
  cacheSolve(firstfun)
  
  # Calculate the duration of this run
  dur1 = Sys.time() - time1
  message("First run")
  print(dur1)
  
  ## SECOND RUN: this time the inverse of the matrix is already in the cache and we should be able to see a reduction in the time needed for the computation
  
  time2 = Sys.time()
  cacheSolve(firstfun)
  dur2 = Sys.time() - time2
  message("Second run")
  print(dur2)
    
}


## Perform the time test on a real matrix
# Generate a matrix with 2000 rows and 2000 column filled with values taken from an Uniform distribution
# The seed needs to be fixed for reproducibility
set.seed(09041953)
u = runif(4000000)
uni_mat = matrix(u, nrow=2000, ncol=2000)

# Run the time test fuction to see it all work
time_test(uni_mat)
