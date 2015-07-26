## Matrix inversion is a costly process , so there may be some benefits to cache the inverse of a matrix.
## This R file contains two functions makeCacheMatrix and cacheSolve. 
## makeCacheMatrix() computes the special matrix object that can cache its inverse.
## cacheSolve() computes the inverse of the special matrix already created by makeCacheMatrix().
## If the inverse has already been calculated and the matrix has not changed, the cachesolve should retrieve the inverse 
## of the matrix.

## Function to create special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        setmatrix<-function(y){
                x<<-y
                inv<<-NULL
        }
        getmatrix<-function()x
        setinverse<-function(inverse)inv<<-inverse
        getinverse<-function()inv
        list(setmatrix=setmatrix,getmatrix=getmatrix,setinverse=setinverse,getinverse=getinverse)
}


## Function to compute the inverse of a matrix.

cacheSolve<-function(x,...) {
        inv<-x$getinverse()
        if(!is.null(inv))
        { message("getting cached data..")
                return(inv)   ## Return the inverse of the matrix 'x' from the cache.
        }
        inputmatrix<-x$getmatrix()
        inv<-solve(inputmatrix)
        x$setinverse(inv)
        inv                 ## Return a matrix that is the inverse of 'x'
        
}
