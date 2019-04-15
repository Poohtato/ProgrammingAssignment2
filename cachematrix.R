## The following functions will create an inverse matrix of a matrix, if none has yet been 
## calculated and cached.

## The first function creates a list that contains four functions to be able to set and get 
## the matrix data and to set and get the inverse of the matrix. If you want to store the 
## inverse matrix later, you have to assign this object to a variable and pass it to the 
## second function. A new matrix could be passed to the object via $set(new_matrix). 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list(set=set, get=get, setinv=setinv,
       getinv=getinv)
}


## The second function takes the type makeCacheMatrix(a_matrix) object as input. If there is 
## already an inverse matrix cached, it will inform you about it and display the cached 
## inverse matrix. Otherwise the function calculates the inverse matrix, caches and also
## displays it. You can access the cached inverse matrix with $getinv(), if you stored 
## the result of the first function in a variable and passed it to the second function.

cacheSolve <- function(x,...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinv(i)
  i
}

