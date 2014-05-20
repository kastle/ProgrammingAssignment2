## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a 
## matrix rather than compute it repeatedly. 


## The first function, makeCacheMatrix creates a list
## containing a function to:
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse matrix
## 4.get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        set<-function(y){
                x<<-y
                i<<-NULL
        }
        get<-function() x
        setinv<-function(inv) i<<-inv
        getinv<-function()i
        list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## The following function computes the inverse of a square matrix.
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse and sets it in the cache via
## the setinv function.

cacheSolve <- function(x, ...) {
        i<-x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data<-x$get()
        i<-solve(data,...)
        x$setinv(i)
        i
}
