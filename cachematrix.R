## the function makeCacheMatrix basically caches the inverse of a matrix 
## the function Cachesolve calculates the inverse of a matrix (either from cache or "from scratch")

##important: 
require("MASS")
require("R.cache")


## makeCacheMatrix:  saves the inverse matrix under a 
##key generated on the basis of the matrix itself

makeCacheMatrix <- function(x) {
  if(class(x)=="matrix"){ ##check if x is of class matrix
      temp_dim <- dim(x)
      temp_dim1 <- temp_dim[1]
      temp_dim2 <- temp_dim[2]
      temp_key <- list(x)
        if(temp_dim2 == temp_dim1) ##check whether the matrix is a square matrix
        {x_inv <- solve(x)} ## solve creates the inverse of a square matrix x
        else(x_inv <- ginv(x))## creates the general inverse matrix 
      saveCache(x_inv, temp_key)}    ##save the inverse of x in cache memory with a certain key
  else(return("Object x has to be of class Matrix"))} ##warning message if x is not of class matrix


##cacheSolve: solves for the inverse matrix of x. if the inverse of x is available
##in the cache memory it will load the result out of cache, else it will calculate the inverse
  
cacheSolve <- function(x) {
  temp_key <- list(x) ##transform x in list in order to create key as saved in previous funtion
  temp_exist <- loadCache(temp_key)  
  if(exists("temp_exist")==TRUE){ ##check whether the inverse for x exists in the cache
    loadCache(temp_key)} ##load the inverse matrix as saved in the cache under a specific key
  else(ginv(x))} ##calculate inverse matrix if it is not available in the cache memory

