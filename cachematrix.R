# The two functions below are used to create a matrix and calculate its inverse,
# caching the inverse for future reference

# This function creates a matrix object that can cache its inverse.
makeCacheMatrix <- function(mat = matrix()) {
		
		# The matrix inverse has not been calculated is is therefore NULL
        mInv <- NULL
        
		# This changes the original matrix (mat) to assign it a new value (y), 
		# and since the matrix has changed, we no longer know its inverse and 
		# mInv is set again to NULL
		set <- function(y) {
			# <<- is used since the values for mat and mInv were defined within
			# the parent environment to the set function (makeCacheMatrix). To
			# change them we need <<- instead of <-
			mat <<- y
            mInv <<- NULL
        }
         
		# Returns the original matrix, mat 
		get <- function() {
			mat
		}
        
		# Sets the inverse matrix, mInv, to whatever is inputted
		setmInv <- function(inverse) {
			mInv <<- inverse
		}
        
		# Returns the inverse of the original matrix. Will return NULL if the
		# inverse hasn't been calculated let
		getmInv <- function() {
			mInv
		}
        
		# Returns a list of functions that can be accessed
		list(set = set, get = get,
              setmInv = setmInv,
              getmInv = getmInv)

}

# This function computes the inverse of the matrix from makeCacheMatrix.
cacheSolve <- function(mat, ...) {
        ## Return a matrix that is the inverse of 'x'
		
		# If the inverse was previously cached, and the matrix has not been
		# changed (which sets mInv to NULL again) then the inverse is retrieved 
		# from the cache
		 mInv <- mat$getmInv()
         if(!is.null(mInv)) {
                 message("Getting cached data")
                 return(mInv)
         }
		 
		 # If the inverse is not stored in the cache (so it is NULL) then the 
		 # inverse is calculated here and then stored
         data <- mat$get()
         mInv <- solve(data, ...)
         mat$setmInv(mInv)
         mInv
		
}