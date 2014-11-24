## dist-funcs.R

## some functions for calculating moments

## kurtosis
# This formulation of kurtosis adjusts it so that if a distribution is normal,
# it will have k=0 (rtaher than 3), if platykurtic k<0, and if leptokurtic k>0.
kurtosis<-function(x, ...) {
	m4<-sum((x-mean(x,...))^4)/sum(!is.na(x))
	s4<-var(x,...)^2
	m4/s4-3
}
# improved function with a rough estimate of standard error. 95% CI = 2*ses
kurtosis2<-function(x) {
	m4<-sum((x-mean(x))^4)/length(x)
	s4<-var(x)^2
	ku <- m4/s4-3
	n1 <- length(x)
	sks <- sqrt(24/n1)
	return(list(ku,sks))}

#kurtosis2(subset(Y0, sp=="Abco")$l) #use for all species
#kurtosis2(subset(Y1, sp=="Abco")$l)	

# Skewness should fall between [-1,1], and a value >0.2 indicates great
# skewness
skewness<-function(x) {
	m3<-(mean(x) - median(x))
	s2<-var(x)
	m3/s2}
	
## skewness(subset(Y0, sp=="Abco")$l)
## skewness(subset(Y1, sp=="Abco")$l)


# improved function with a rough estimate of standard error. 95% CI = 2*ses
skewness2<-function(x) {
	m3<-(mean(x) - median(x))
	s2<-var(x)
	n1 <- length(x)
	sk <- m3/s2
	ses <- sqrt(6/n1)
	return(list(sk,ses))
}

#skewness2(subset(Y0, sp=="Abco")$l)
#skewness2(subset(Y1, sp=="Abco")$l)



