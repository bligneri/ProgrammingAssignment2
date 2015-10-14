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
  
  # setinverse : set the inverse value
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
  
  ## Look for the already computed inverse
  i <- x$getinverse()
  
  ## if not null ! Yahoo: the computation has been cached. Let's use it !
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## Else we have to compute it 
  data <- x$get()
  
  ## We compute the inverse using solve()
  i <- solve(data)
  
  ## We set the inverse value so that it is now ... cached.
  x$setinverse(i)
  
  ## We return the recently computed inverse value
  i
}


makeTest<-function(){

## How to test :
## Create a 2x2 matrix
test<-matrix(nrow=2,ncol=2)
test[1,1]=10
test[1,2]=1
test[2,1]=-2
test[2,2]=2

message("This is our test matrix :")
print(test)

message("We create a CacheMatrix")
testMatrix<-makeCacheMatrix(test)

message("Display the CacheMatrix")
print(testMatrix$get())

message("The inverse is not set :")
print(testMatrix$getinverse())
# NULL : Not set

message("We call cacheSolve")
cacheSolve(testMatrix)

message("The inverse has been computed and is set")
print(testMatrix$getinverse())


message("We call cacheSolve again")
cacheSolve(testMatrix)
message("Inverse was cached : no computation, only getinverse() is called")
message("The inverse is the same as precedently")
print(testMatrix$getinverse())
        
}
