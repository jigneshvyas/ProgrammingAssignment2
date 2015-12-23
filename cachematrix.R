## R Programming
## Programming Assignment 2 - Lexical scoping
## Author : Jignesh Vyas


## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object, which is really a list 
## containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        
        setinverse <- function(inverse) m <<- inverse
        
        
        getinverse <- function() m
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}



## The following function calculates the inverse of the special "matrix" created
## with the above function.
## Before calculating inverse it first checks in the cache,
## if the cache returns null, it will calculate the inverse,
## and put it in the cache. Next time when inverse is requested,
## it will get it from cache.


cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        
        ## check if inverse is in the cache
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data") ## if exists return inverse from the cache
                return(m)
        }
        
        data <- x$get()
        m <- solve(data, ...) ## if not in cache, calculate the inverse
        x$setinverse(m) ## store the inverse in the cache for sub-sequent use
        
        m
        
}

## to test the  above functions
## create square matrix
mat<-matrix(c(1,2,3,4),2,2)

## call first function makeCacheMatrix
matx<-makeCacheMatrix(mat)

## call to create inverse of the matrix
## since this is first call, it will return inverse using solve and put it in the cache
cacheSolve(matx)

## this second call will return inverse from the cache
cacheSolve(matx)
