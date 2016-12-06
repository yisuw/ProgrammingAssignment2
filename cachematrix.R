## This script illustrates cache of  the inverse of a matrix

## This function creates a matrix object that can cache its inverse
## Assume input matrix is squre invertible. 
## x is the input matrix, i is the inverse of x
makeCacheMatrix <- function(x = matrix()) {
    i<-NULL              #set inverse to be NULL
    set <- function(y) { #set given matrix parameter to be input matrix
        x <<- y
        i <<- NULL
    }
    get <- function() x #retrieve matrix
    setinverse <- function(inverse) i <<- inverse #set matrix inverse using
                                                  #cacheSolve
    getinverse <- function() i                    #retrieve inverse
    list(set = set, get = get,                    #make list
         setinverse = setinverse,
         getinverse = getinverse)
}


## Assume input matrix is a squre invertible matrix, this funciton
## computes the inverse of matrix x if it's not in cache. It fetches
## the inverse of matrix x if it's been computed before. 
cacheSolve <- function(x, ...) {
        i<-x$getinverse()               #fetch inverse
        if(!is.null(i)){                #if inverse exists, then return
            message("getting cached data")
            return(i)
        }
        #if inverse is null, computer inverse
        data<-x$get()
        i<-solve(data, ...)
        x$setinverse(i)     #store computed inverse
        i
}
