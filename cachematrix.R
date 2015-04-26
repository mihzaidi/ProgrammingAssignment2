## Put comments here that give an overall description of what your
## functions do

## creates and sets the values of the matrix
}
makeCacheMatrix <- function(x = matrix()) {
  m <-NULL  #value of m in the global environment is set to null.
  set <- function(y) { #sets the matrix value
    x <<- y   #transfers cache value of matrix to x inside the set function
    m <<- NULL #sets the value of m to null in the containing environment
  }
  get <- function() x #gets the value of x currently set in containing environment
  setmatrix <- function(solve) m<<- solve #sets the value of the solution to m in the containing environment for the inverse to be done
  getmatrix<-function() m #gets the matrix m that has been set before
  list(set=set, get=get,       #creates a list to contain all four functions
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}
        ## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix() #if the inverse is set, this gets it
  if(!is.null(m)){ #checks to see if the function CacheSolve has been run already
    message("getting cached data") #checks for changes in matrix and if not then returns cached data.
    return(m)
  }
  matrix<-x$get()# otherwise this gets the input value of matrix
  m<-solve(matrix, ...) #computes the value of the inverse
  x$setmatrix(m) #cahces the inverse
  m #returns the inverse
}
