#' Euclidean Algorithm
#'
#' @param a A numeric scalar or integer
#' @param b A numeric scalar or integer
#'
#' @return   Greatest common divisor of a and b
#' @export    
#'
#' @examples  euclidean(123612, 13892347912) 
#'            euclidean(100, 1000)
#@source \url{https://en.wikipedia.org/wiki/Euclidean algorithm}
#' 
euclidean<- function(a,b){
  
  stopifnot(is.numeric(a)==TRUE, is.numeric(b)==TRUE)
  temp<-0
  m2<- min(abs(a),abs(b))
  m1<- max(abs(a),abs(b))
  while (m2 != 0) {
    temp<- (m1 %% m2)
    m1<- m2
    m2<- temp
  }
  return(m1)
  
}



