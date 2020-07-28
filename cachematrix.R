## This is a set of functions to create a matrix then solve it's inverse and 
## keep that inverse for easy access.  

## This first function creates then Caches a Matrix

makeCacheMatrix <- function (x = matrix()){
        I<-NULL
        set <- function(y){
                x<<-y
                I<<-NULL
        }
        get <- function() x
        setinverse <- function(inverse) I<<- inverse
        getinverse <- function()I
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        }

## This function solves the inverse for a matrix and returns it's value... 
## Alternatively if it's already been solved it provides the solved value.  Make sure your matrix is invertable.

cacheSolve <-function(x,...){
        I<-x$getinverse()
        if(!is.null(I)){
                message ("getting cached data")
                return(I)
        }
        matr <- x$get()
        I<-solve(matr, ...)
        x$setinverse(I)
        I
}
