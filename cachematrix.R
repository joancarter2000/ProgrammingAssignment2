## This R file contains two funcitons for cashing the inverse of a matrix

## makeCacheMatrix function creates a special matrix object that can cache its inverse. This function defines a list of 4 
##functions to store and retrieve the inverse matrix of the special matrix object.
##makeCacheMatrix function takes a matrix argument and converts it into a special matrix object. If no argument is passed, 
##the set function can be used to assign a matrix later.

makeCacheMatrix <- function(x = matrix()) {
 
  m<-NULL                        ## initiate m value to NULL
  
  set<-function(y){              ## set the matrix need to inverse
  x<<-y                          
  m<<-NULL                       ## clear the cashe
  }
  get<-function()x               ## get the matrix need to inverse
  setInverse<-function(solve) m<<- solve            ## store the calculated inverse matrix in cashe
  getInverse<-function()m                           ## get the inverse matrix stored in the cashe
  list(set=set, get=get,setInverse=setInverse,getInverse=getInverse)
}


## cacheSolve function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix. If the inverse has already been calculated (and the matrix has not been changed), then the cacheSolve should 
##retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getInverse()                 ## retrive whatever inverse matrix from the cache              
            if(!is.null(m)) {               ##checking if the retrived inverse matrix is present
                    message("getting cached data")
                    return(m)               ##inverse matrix is in cache and return its value directly
            }
            data <- x$get()                 ##inverse matrix not in cache so need get the input matrix for calculation
            m <- solve(data)                ##calculate the inverse matrix
            x$setInverse(m)                 ##store the inverse matrix in cashe
            m                               ##return the inverse matrix
}
