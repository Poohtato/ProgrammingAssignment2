## The following two functions will create an inverse matrix of a matrix passed as
## an argument to the first function, if no inverse matrix has yet been calculated before. 
## The inverse matrix will be estimated within the second function and can be stored in 
## an object of the type of the first function 'makeCacheMatrix'.
## If an inverse matrix has already been calculated and cached, then the second function
## will just inform you about that fact and return the cached inverse matrix.

## The first function 'makeCacheMatrix' creates a matrix object x that will store the 
## information from the passed input matrix (e.g. 'a_matrix') when calling the function: 
## makeCacheMatrix(a_matrix)
## Furthermore, a variable i is created, in which the inverse of that matrix can be stored.
## In the end, the function will create a list that contains four functions set(), get(), 
## setinv() and getinv() to be able to set and get the matrix data and to set=save and 
## get=call the cached inverse of the matrix. Any time the set() function is being
## executed - whether it is by calling the makeCacheMatrix function or by assigning a
## new matrix to the function from outside - the value of the inverse matrix i is set to 
## 'NULL'. When assigning the values to the variables i and x within the functions, the 
## '<<-' operator is being used and thus the data will not only be available in the 
## environment, in which the values have been assigned to the variables. You can access 
## the data stored in x and the inverse of the matrix cached in i from outside of the 
## function makeCacheMatrix (only) via these four functions. 
## So, if you want to store your matrix and the inverse matrix, you need to call the 
## function and save the result list by assigning it to a variable:
## p <- makeCacheMatrix(a_matrix). 
## Now you can access the input data (a_matrix) with p$get(). There is no inverse matrix
## yet, because i has been set to 'NULL' and the second function 'CacheSolve' has not 
## been used so far, so the result of p$getinv() will be 'NULL' right now. 
## Any time that you call the function makeCacheMatrix on a matrix, all variables will be
## set anew and i set to 'NULL', even if you are using the same data 'a_matrix' and assign
## the result list to the same variable 'p' as before.


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


## The second function 'CacheSolve' takes the makeCacheMatrix(a_matrix) as input and 
## evaluates whether an inverse matrix has already been created and stored in the 
## getinv() function within the makeCacheMatrix function. This can only be the case, 
## when you pass the result of makeCacheMatrix(a_matrix) stored in a variable: CacheSolve(p). 
## So p will be an object of type 'makeCacheMatrix(x)'. 
## If there is already an inverse matrix stored and the matrix has not changed by setting
## the data to a new matrix from outside with p$set(new_matrix), then it will give you 
## the info "getting cached data" and display the cached inverse matrix. 
## If no inverse matrix has yet been calculated, then the CacheSolve function will 
## calculate the inverse matrix based on the input data - that the function will retrieve 
## by using the p$get() operator. Then, the CacheSolve function will calculate the inverse
## matrix with the solve() function and store the result internally in i, as well as 
## within p - accessing the cache with p$setinv(i). 
## If you call CacheSolve twice on p, it will display that the function got cached data 
## and will show the cached inverse. As described above, any time that you call the 
## makeCacheMatrix function anew, even when using the same data, the function will set all 
## entries anew and store them in the variable and its four functions.
## So after setting p anew to the very same matrix, p$getinv() will be NULL again, thus the 
## function CacheSolve(p) will not get cached data and will (re-)calculate the 
## inverse matrix.
## Even when the data has already been stored and the inverse matrix has been estimated 
## and cached within p, you can still change the input data by passing a new matrix with 
## p$set(new_matrix) from outside without having to rerun the function makeCacheMatrix. 
## By doing this, the data will be set to "new_matrix" and the old inverse matrix will 
## be deleted and i set to "NULL" so that when you use CacheSolve on p, the inverse matrix 
## will be recalculated on the new data and cached. You can access the new matrix as well
## as the new data, again by calling p$get() and p$getinv() respectively.


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

