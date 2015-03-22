## Programming Assignment 2: Lexical Scoping
## Edwin Torres

# For testing the functions please use the code 
# commented and the end of the file


# makeCacheMatrix: This function creates a special "matrix" object 
# that can cache its inverse.
# x matrix
# I inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  
  I <- NULL
  
  set <- function(y) {
    x <<- y
    #Reset the inverse matrix I
    I <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) I <<- inverse
  
  getInverse <- function() I
  
  list(set = set
        ,get = get
        ,setInverse = setInverse
        ,getInverse = getInverse)
}


# cacheSolve: This function computes the inverse of the special 
# "matrix" returned by makeCacheMatrix above. If the inverse has 
# already been calculated (and the matrix has not changed), then 
# the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  I <- x$getInverse()
  
  if(!is.null(I)) {
    message("Getting cached data.")
    return(I)
  }
  
  data <- x$get()
  
  #I <- solve(data, ...)
  I <- solve(data) %*% data
  
  x$setInverse(I)
  
  I
}

##########################################
### Code for testing the functions
### Uncomment the lines of code 
### to test the functions


## The following matrix M was obteined from: 
## http://www.purplemath.com/modules/mtrxinvr.htm
## You can chech the inverse matrix on the URL
#M <- matrix(c(1,1,1,3,4,2,3,3,4),3,3)

## Create the object makeCacheMatrix named y
#y <- makeCacheMatrix()

## Set the matrix M to object y
#y$set(M)

## Get the matrix M from y to test the function
#y$get()

## Test getInverse, in this step must be null
#y$getInverse()

## Calculate inverse matrix of y for the first time
#cacheSolve(y)

## Test function getInverse
#y$getInverse()

## Calculate inverse matrix of y for the second time
## this time the cached data
#cacheSolve(y)

