## Put comments here that give an overall description of what your
## functions do
      # This function calculates the inverse of a matrix, retrieving the value
      # from the cache if the inverse has already been computed.

## Write a short comment describing this function
      # First we create a function that will cache de matrix
      # Then we compute de inverse, conditioning to a null value of the
      # cached inverse

## The following function will create a matrix object
      # Creates a matrix object that can be made available in the cache


makeCacheMatrix <- function(x = matrix()) {

      inv_mat <- NULL         #iniatialize matrix variable

      set <- function(y) {    #set the matrix value
            x <<- y           #assigns value in the parent level
            inv_mat <<- NULL  #assigns value in the parent level
      }

      get <- function() {x}   #get the matrix value

      setInverse <- function(inverse) {inv_mat <<- inverse} #set de matrix inverse value

      getInverse <- function() {inv_mat} #get the matrix inverse value

      list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)

}

## Write a short comment describing this function
   #This function will evaluate if there is already a cached inverse matrix,
   #in case there is, it will return the value, if not, it will calculate the value
   #and store it in the cache for the next time it is needed.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv_mat <- x$getInverse() #assigns to inv_mat a matrix inverse of x

      if(!is.null(inv_mat)) { #is the inverse already computed?
            message("getting cached data / obteniendo datos del cache")
            return(inv_mat)
      }
      mat <- x$get()
      inv_mat <- solve(mat, ...) #computes the inverse of the matrix
      x$setInverse(inv_mat)
      inv_mat
}
