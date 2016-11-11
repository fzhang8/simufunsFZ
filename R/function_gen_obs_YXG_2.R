
generate_obs_YXG <- function(Gmode=c("continuous","discrete"),
															n=20,Gcolmn=6,Xcolmn=8,actvXcolmn=4,actvGcolmn=3,
															laty,latx,latg,var_obs_y=1,var_obs_x=1,var_obs_g=1,
															p0hat=0,p1hat=0,p2hat=0,Xcenter=FALSE,Xmean=c("obsmean","latmean"),
															Gcenter=FALSE,Gmean=c("obsmean","latmean")){
  set.seed(123456)
  Gmode <- tolower(Gmode)
  Gmode <- match.arg(Gmode)
  Xmean <- tolower(Xmean)
  Xmean <- match.arg(Xmean)
  Gmean <- tolower(Gmean)
  Gmean <- match.arg(Gmean)
  
  
  if(Gmode == "continuous"){
		p <- Gcolmn
	  q <- Xcolmn
  	m <- actvXcolmn
  	h <- actvGcolmn
  
	  ######## generate G matrix ############
	  G <- matrix(0,n,p)
	  G <- apply(G,1,function(x) x <- c(rnorm(h,latg,sqrt(var_obs_g)),rnorm(p-h,latg+2,10)))
	  G <- t(G)
	  if(Gcenter){
	  	if(Gmean == "obsmean"){
	  		G <- demean(G)
	  	}else{
	  		G <- demean(G,c(rep(latg,h),rep(latg+2,p-h)))
	  	}
	  }
	  #######################################
  
	  ########## generate X matrix ##########
	  X <- matrix(0,n,q)
	  X <- apply(X,1,function(x) x <- c(rnorm(m,latx,sqrt(var_obs_x)),rnorm(q-m,latx+2,10)))
	  X <- t(X)
	 	if(Xcenter){
	  	if(Xmean == "obsmean"){
	  		X <- demean(X)
	  	}else{
	  		X <- demean(X,c(rep(latx,m),rep(latx+2,q-m)))
	  	}
	  }
	  #######################################
  
	  ########### generate Y vector ########
	  Y <- sapply(rep(0,n),function(x) x <- rnorm(1,laty,sqrt(var_obs_y)))
	  Y <- matrix(Y,n,1)
	  ########################################
 		
	}else{ ### discrete 
	
		p <- Gcolmn
	  q <- Xcolmn
  	m <- actvXcolmn
  	h <- actvGcolmn
  
	  ######## generate G matrix ############
	  G <- matrix(0,n,p)
	  G <- apply(G,1,function(x) x <- c(sample(c(0,1,2),h,replace = TRUE,prob = c(p0hat,p1hat,p2hat)),sample(c(0,1,2),p-h,replace=TRUE,prob=c(0.333,0.333,0.333))))
	  G <- t(G)
	  if(Gcenter){
	  	if(Gmean == "obsmean"){
	  		G <- demean(G)
	  	}else{
	  		G <- demean(G,c(rep(p1hat + 2 * p2hat,h),rep(0.33+2*0.33,p-h)))
	  	}
	  }
	  #######################################
  
	  ########## generate X matrix ##########
	  X <- matrix(0,n,q)
	  X <- apply(X,1,function(x) x <- c(rnorm(m,latx,sqrt(var_obs_x)),rnorm(q-m,latx+2,10)))
	  X <- t(X)
	  if(Xcenter){
	  	if(Xmean == "obsmean"){
	  		X <- demean(X)
	  	}else{
	  		X <- demean(X,c(rep(latx,m),rep(latx+2,q-m)))
	  	}
	  }
	  #######################################
  
	  ########### generate Y vector ########
	  Y <- sapply(rep(0,n),function(x) x <- rnorm(1,laty,sqrt(var_obs_y)))
	  Y <- matrix(Y,n,1)
	  ########################################
	}
  

  return(list(Y = Y,X = X,G = G))
}

