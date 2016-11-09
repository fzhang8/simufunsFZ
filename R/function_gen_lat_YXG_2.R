#library(MASS)

simuLatYXG <- function(Gmode=c("continuous","discrete"),betay_xg=3,betay_gx=3,betax_g=3,
										var_yxg=1,var_xg=1,g=1,var_g=1,p=0.2,M=100){
  set.seed(123456)
  
	Gmode <- tolower(Gmode)
  Gmode <- match.arg(Gmode)

	genYXG <- function(betay_xg,betay_gx,betax_g,var_yxg,var_xg,g,var_g){
  	a <- betay_xg
  	b <- betay_gx
  	c <- betax_g
  	sig1 <- var_yxg
  	sig2 <- var_xg
  	varg <- var_g
  	B <- diag(c(1,1,1))
		B[1,2] <- -a
		B[1,3] <- -b
		B[2,3] <- -c
    D <- diag(c(sig1,sig2,varg))
   	sigma <- solve(B) %*% D %*% solve(t(B))	
   	E <- c(a*c*g + b*g,c*g,g)
   	Z <- mvrnorm(1,E,sigma)
   	Z <- data.frame(y=Z[1],x=Z[2],g=Z[3])
   	names(E) <- c("Y","X","G")
   	cat("Expection: \n", paste(c("Y","X","G"),E,collapse=", ",sep = ": "),"\n\n")
   	cat("Latent value generated:\n")
   	print(round(unlist(Z),3))
   	cat("\n")
   	return(list(Z=Z,Sigma=sigma,Expecation = E))
	}

	if(Gmode == "continuous"){
		out <- genYXG(betay_xg,betay_gx,betax_g,var_yxg,var_xg,g,var_g)
		return(out)
	}else{ ### discrete variance = 0
		var_g <- 0
		q <- 1 - p
		p0 <- p^2
		p1 <- 2 * p * q
		p2 <- q^2
		phat <- rmultinom(1, M, prob = c(p0,p1,p2)) / M
		p0hat <- phat[1]
		p1hat <- phat[2]
		p2hat <- phat[3]
		ExpectG <- p1hat + 2 * p2hat
		g <- ExpectG
		out <- genYXG(betay_xg,betay_gx,betax_g,var_yxg,var_xg,g,var_g)
		return(c(out,p0hat=p0hat,p1hat=p1hat,p2hat=p2hat,ExpectG=ExpectG))
	}
}
