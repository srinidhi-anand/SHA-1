# SHA-1
makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set(x), get = get(),
             setmean = setmean(mean(x)),
             getmean = getmean())
}
cachemean <- function(x, ...) {
        x <- makeVector(x)
        m <- x$getmean
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean
        m
}
