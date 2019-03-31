## using the <<- operator which lets us access environments which otherwise would be beyond the scope of the object 
##we aim to simply the the process of finding the inverse of a matrix. Because values are constant rather than 
##calculating the inverse we store and use it otherwise known as caching

##As shown in the mean exapmle given in the assignment this function performs corrolary functions
##provide the value of the matrix given as input store it so it can be used again similarly
##gets the inverse of the input matrix and stores it

makeCacheMatrix <- function(x = matrix())
  {
    inv<-NULL
    set<-function(y) 
      {
        x<<-y
        inv<<-NULL
      }
    get<-function() x
    getinverse<-function() inv
    setinverse<-function(inverse) inv<-inverse
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  }


##This function creates the inverse of the input matrix but first checks in the given fn above of the inverse has 
##been calculated

cacheSolve <- function(x, ...)
  {
    inv<-x$getinverse()
    if(!is.null(inv))
      {
        return(inv)
      }
    data<-x$get()
    inv<-solve(data)
    x$setinverse(inv)
    inv    
}


