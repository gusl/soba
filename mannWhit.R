## accepts {B,W}*  rather than {0,1}*
mannWhitneyU2 <- function(s){
  mannWhitneyU(sapply(s, function(c){
    if(c=="W") return(1)
    else return(0)
  }))
}

##input: a ranking containing some Ms
##output: U221 - U41
deltaU <- function(ranking){
  s221 <- sapply(ranking,function(c){ifelse(c=="M","B",c)})
  s41 <- sapply(ranking,function(c){ifelse(c=="M","W",c)})
  U221 <- mannWhitneyU2(s221)
  U41 <- mannWhitneyU2(s41)
  list(U221-U41)
}
