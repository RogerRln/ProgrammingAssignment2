## In this script we create two functions: The first function 'makeCacheMatrix'
## creates a special "matrix" object that can cache its inverse. The second
## function 'cacheSolve' calculates the inverse of the special 'matrix' or 
## retrieve this value from the cache of the first function.

## Example: 
## x<- matrix(1:4, 2, 2)
##      [,1] [,2]
##[1,]    1    3
##[2,]    2    4



## 'makeCacheMatrix' creates a special "matrix" object that can cache its inverse,
## for this task, the'makeCacheMatrix' function contain sub-functions that set/get
## the value of the square matrix and set/get the value of its inverse.

makeCacheMatrix <- function(x = matrix()) {
		inverse <- NULL
        	set <- function(y) {
                x <<- y
                inverse <<- NULL
                }
       	        get <- function(){ x }
                setinverse <- function(solve){ inverse <<- solve }
       	        getinverse <- function(){ inverse }
                list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)

}
rs<- makeCacheMatrix(x)

## 'cacheSolve' calculates the inverse of the special 'matrix' or retrieve this value
## from the cache of the first function ('makeCacheMatrix'), for this task, 'cacheSolve'
## checks if the inverse has already been calculated, if so cacheSolve should retrieve
## the inverse from the cache.

cacheSolve <-function(x,...){
        inverse <-x$getinverse()
        if(!is.null(inverse)){
                message("getting cached data")
                return(inverse)
        }
        data <-x$get()
        inverse <- solve(data,...)
        x$setinverse(inverse)
        inverse
        
}

cacheSolve(rs)

## Result:
## > cacheSolve(rs)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
