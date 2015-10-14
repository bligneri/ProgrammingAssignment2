## Put comments here that give an overall description of what your
## functions do

## This function create a special "Matrix", which is a list containing a function to
## 1. set the value of the matrix
## 2. return the value of the matrix
## 3. set the inverse of the matrix
## 4. return the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  # Set the value of the matrix
  set <- function(y) {
    # Change the value of x to y
    x <<- y
    # Reset the inverse because x has changed
    inverse <<- NULL
  }
  
  # return the value of the matrix
  get <- function()
    x
  
  # setinverse
  setinverse <- function(newinverse)
    inverse <<- newinverse
  
  #getinverse
  getinverse <- function()
    inverse
  
  list(
    set = set, get = get, setinverse = setinverse,getinverse = getinverse
  )
}


## This function look for the cache of the already inversed matrix
### If found : return the cache (already calculated inverse with solve or set with setinverse)
### If not found :
#### compute the inverse using solve()
#### cache the inverse (using setinverse())
#### return the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}

## How to test :
## Create a 2x2 matrix
test<-matrix(nrow=2,ncol=2)
test[1,1]=10
test[1,2]=1
test[2,1]=-2
test[2,2]=2

# Create a testMatrix
testMatrix<-makeCacheMatrix(test)
# Display the matrix
testMatrix$get()

testMatrix$getinverse()
# NULL : Not set

cacheSolve(testMatrix)
# Compute : no cache

cacheSolve(testMatrix)
# Cached : no computation, only getinverse() is called

makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function()
    x
  setmean <- function(mean)
    m <<- mean
  getmean <- function()
    m
  list(
    set = set, get = get,
    setmean = setmean,
    getmean = getmean
  )
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}
