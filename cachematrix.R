##This set of functions create a cached matrix that can be inversed,
##by creating two main functions (makeCacheMatrix and cacheSolve).


## The funtion makeCacheMatrix creates a matrix object that can be inversed 
## by looping 4 nested functions (set, get, setinverse and getinverse). 

makeCacheMatrix <- function (x = matrix()) {             
        inv <- NULL                                     
        set <-function (y){                              
                x <<- y
                inv <<- NULL
        }
        get <- function () x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function () inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The function cacheSolve uses the functions created in makeCacheMatrix 
## to return a matrix that is  in the inverse of x. If the inverse of the
## matrix has already been computed, a messgae "getting cached data" will
## be returned along with the inverted matrix.

cacheSolve <- function (x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

## for example

dom_matrix <- makeCacheMatrix(matrix(1:6, 2,2))
dom_matrix$get()
##      [,1] [,2]
##[1,]    1    3
##[2,]    2    4

cacheSolve(dom_matrix)

##      [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

cacheSolve(dom_matrix)
 ##getting cached data
   ##      [,1] [,2]
   ##[1,]   -2  1.5
   ##[2,]    1 -0.5

