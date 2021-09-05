


#' Euclidean
#'
#' @param x1 is an integer
#' @param x2 is an integer
#'
#' @return The function returns the blablabl
#' @export
#'
#' @examples
#' euclidean(100,1000)
euclidean <- function(x1, x2){



  if(!is.numeric(x1)){
    stop("x1 is not numeric")
  } else if(x1 %% 1 != 0) {
    stop("x1 is not an integer")
  }

  if(!is.numeric(x2)){
    stop("x2 is not numeric")
  } else if(x2 %% 1 != 0){
    stop("x2 is not an integer")
  }


  if(x1 > x2) stop("Not a valid input. x1 is larger than x2")


  while(x2 != 0){ #x2 är b och x1 är a från pseudokod.
    t <- x2
    x2 <-  x1 %% x2
    x1 <- t


  }

  return(max(x1,-x1))


}



print("hejsan")
