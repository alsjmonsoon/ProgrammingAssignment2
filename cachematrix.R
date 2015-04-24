## This function creates a special "matrix" object that can cache its inverse.
##Matrix inversion is usually a costly computation and there may be some benefit
##to caching the inverse of a matrix rather than computing it repeatedly.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        set<-function(y=matrix()) {
                x<<-y
                i<<-NULL
        }
        get<-function() x
        setinverse<-function(solve) i<<-solve
        getinverse<-function() i
        list(set=set, get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. It first checks to see if the inverse has already been
## calculated or the matrix has not changed. If so, the cacheSolve should retrieve 
##the inverse from the cache. Otherwise, it caculates the inverse and return the 
## inversed matrx.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i<-x$getinverse()
        if (!is.null(i)){
               message("getting cached data") 
               return(i)
        }
        data<-x$get()
        i<-solve(data,...)
        x$setinverse(i)
        i
}
