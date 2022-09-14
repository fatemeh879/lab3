euclidean<- function(a,b){
  
  if(a==0 | b==0)
    stop()
  temp<-0
  m2<- min(a,b)
  m1<- max(a,b)
  while (m2 != 0) {
    temp<- (m1 %% m2)
    m1<- m2
    m2<- temp
  }
  return(m1)
  # return(m2)
}

euclidean(123612, 13892347912)
euclidean(100, 1000)

