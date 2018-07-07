## makeCacheMatrix is a function that does the following: 
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse matrix
## 4. Get the value of the inverse matrix 

## cachesolve is a function that actually computes the inverse matrix using 'solve' function. 
## It also tells you whether the result is coming from the cached data.

makeCacheMatrix = function(x=matrix()){
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) i <<- inverse
    get_inverse <- function() i
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
    
}

cachesolve <- function(x, ...) {
    i <- x$get_inverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$set_inverse(i)
    i
}

## Below is to test how it actually works. 

k <- makeCacheMatrix()
k$set(matrix(c(2,7,2,4),2,2))
k$get()
cachesolve(k)
k$get_inverse()

