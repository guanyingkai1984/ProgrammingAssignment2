##library(MASS) is used to calculate inverse of matrices using function ginv()
library(MASS)

## First Function makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) 
{
  #initializing inverse as NULL  
  inv<-NULL            
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  #function to get matrix x
  get<-function()x             
  setinv<-function(inverse)inv<<-inverse
  getinv<-function(){ 
    #function to obtain inverse of the matrix
    inv<-ginv(x)
      
  }
  list(set = set, 
       get = get, 
       setinv = setinv, 
       getinv = getinv)
}


##This is used to get the cache data

#gets cache data 
cacheSolve <- function(x, ...)      
{
  inv<-x$getinv()
  #checking whether inverse is NUll  
  if(!is.null(inv))
    {                  
    message("getting cached data!")
    #returns inverse value
    return(inv)                       
    }
  
  data<-x$get()
  #calculates inverse value
  inv<-solve(data,...)              
  x$setinv(inv)
  # Return a matrix that is the inverse of 'x'
  inv   
}