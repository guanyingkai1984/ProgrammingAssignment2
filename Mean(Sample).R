makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(makeVector.object, ...) {
  m.local <- makeVector.object$getmean()
  if(!is.null(m.local)) {
    message("getting cached data")
    return(m.local)
  }
  data <- makeVector.object$get()
  m.local.calculated <- mean(data, ...)
  makeVector.object$setmean(m.local.calculated)
  m.local.calculated # return the mean value
}
