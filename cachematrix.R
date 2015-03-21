# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. 

#This is especially true for very large matrices

#Two functions are used to cache the inverse of a matrix.

#The first function, makeCacheMatrix creates a special "matrix", 
#which is really a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse

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

# The following function checks if the inverse has already been computed. 
#If so, it gets the result and skips computation.
# Othwerwise it computes the inverse, sets the value in the cache 
# The matrix has to be always invertible.

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


# sample execution of the data
#> x<-matrix(c(2,3,2,2),2,2)
#> m = makeCacheMatrix(x)
#> m$get()
#[,1] [,2]
#[1,]    2    2
#[2,]    3    2
#> cacheSolve(m)
#[,1] [,2]
#[1,] -1.0    1
#[2,]  1.5   -1
#> cacheSolve(m)
#getting cached data
#[,1] [,2]
#[1,] -1.0    1
#[2,]  1.5   -1

