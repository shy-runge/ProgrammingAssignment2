
#The first function, makeCacheMatrix creates a special "matrix", 
#which is really a list containing a function to
#set the value of the vector
#get the value of the vector
#set the value of the mean
#get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
        minv <- NULL
        set <- function(y) {
                x <<- y
                minv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) minv <<- solve
        getinverse <- function() minv
        list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        minv <- x$getinverse()
        if(!is.null(minv)) {
                message("getting cached data")
                return(minv)
        }
        data <- x$get()
        minv <- solve(data, ...)
        x$setinverse(minv)
        minv
        
}
