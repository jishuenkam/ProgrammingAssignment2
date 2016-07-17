## We want to create a function such that for an invertible matrix, 
## it will return a cached matrix inverse. 

## makeCacheMatrix function takes an invertible matrix 'x' as an argument, 
## and return a list of functions that do the following:
## 1) Set the value of the matrix. 
## 2) Get the value of the matrix.
## 3) Set the inverse of the matrix.
## 4) Get the inverse of the matrix.


makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL       
    set <- function(y){
        x <<- y      
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The following function calculates the inverse of the matrix based on the 
## previous function. It first checks if the inverse has been computed. If so,
## it skips the computation and returns the cached inverse. Otherwise, it 
## computes the inverse of the marix and set the value of the inverse via the 
## setinverse function.

cacheSolve <- function(x, ...) {
        
    i <- x$getinverse()
    if(!is.null(i)){
        print("return the cached matrix inverse")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)     
    i 
}
