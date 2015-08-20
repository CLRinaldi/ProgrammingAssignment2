makeCacheMatrix <- function(x = matrix()) { ##this function caches a matrix to be called by 'get' 
  i <- NULL                                #function and then takes the inverse of the called
  set<-function(y){                         #matrix, by the nested 'set' function, which defines 
    x<<-y                                   #y as = x  
    i<<- NULL                             
  }                                        
  
  get<-function() x                         ##calls the cached matrix and assigns it variable x
  setinverse<-function(solve) i<<- solve    ##assigns the solve function to 'i'in makeCachematrix
  getinverse<-function() i                  ##calls the inverse matrix by assigning it to 'i' in 
  list(set=set, get=get,                    #makeCachematrix function
       setinverse=setinverse,
       getinverse=getinverse)
}

cacheSolve <- function(x=matrix(), ...) { 
  i<-x$getinverse()            ##argument 'i' in function cacheSolve calls the inverse matrix
  if(!is.null(i)){             ##if getting the inverse matrix                                
    message("getting cached data") ##produces the specified message the result of argument 'i'      
    return(i)                      #the inverse is returned        
  }
  matrix<-x$get()              ##argument 'matrix' = calls original matrix
  i<-solve(matrix, ...)        ##argument i = inverse of called matrix
  x$setinverse(i)              ##assigns the argument solve to 'i'
  i                            ##retruns the inverse matrix
}
