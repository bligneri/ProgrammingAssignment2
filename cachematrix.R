## Put comments here that give an overall description of what your
## functions do

## This function create a special "Matrix", which is a list containing a function to
## 1. set the value of the matrix
## 2. returne the value of the matrix
## 3. 

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	# Set the value of the matrix
        set <-function(y){
                # Change the value of x to y
		            x <<- y
		            # Reset the inverse because x has changed
                inverse <<- NULL
        }
	
	# return the value of the matrix
        get <-function() x

	# setinverse
        setinverse <- function(newinverse) inverse <<- newinverse
        
        #getinverse
	getinverse <- function() inverse
        
        list(set=set, get=get, setinverse=setinverse,getinverse=getinverse)
}


## This function look for the cache of the already inversed matrix
### If found : return the cache (already calculated inverse with solve or set with setinverse)
### If not found : compute the inverse, cache the inverse and return the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
            message("getting cached data")
            return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}



makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}

