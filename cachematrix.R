## makeCacheMatrix creates a special object out of a matrix, being a list of 4 elements
## cacheSolve determines the inverse of a matrix. the matrix is first to be transferred into the list by the function makeCacheMatrix

## create the list
## list <-makeCacheMatrix(matrix)
## determine the inverse of the matrix using the list
## cacheSolve(list)

makeCacheMatrix <- function(x = matrix()) {
  # x is a square invertible matrix
  # function creates a list of 4 elements
  # list of 4 functions.Elements:
  # 1: initialise cache; both matrix x and inverse into cache 
  invM <- NULL
  set <- function(y) {
    x <<- y
    invM <<- NULL
  }
  # 2: fetch matrix x and put result in second element $get
  get <- function() x
  # 3: put the inverse of matrix x (inversM)in thirth element $setinv
  setinv <- function(inversM) invM <<- inversM
  # 4: fetch inverse of matrix x and put result in fourth element $getinv
  getinv <- function() invM
  # create the list
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

 cacheSolve <- function(x, ...) {
  # x is the list made by function makeCacheMatrix.R out of a matrix
  # get inverse of matrix out of the list x
  invM <- x$getinv()
  # check if the inverse is availabe in the list. Return inverse
  if(!is.null(invM)) {
    message("getting cached inverse matrix")
    return(invM)
  }
  # determine the inverse of the matrix and put in cache
  mat <- x$get()
  invM <- solve(mat, ...)
  x$setinv(invM)
  # return inverse calculated
  invM
}
