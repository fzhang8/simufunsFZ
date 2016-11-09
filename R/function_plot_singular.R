#library(ggplot2)

singular_plot <- function(singulars){
	d <- data.frame(Singular_value = singulars,Index = seq(1,length(singulars)))
	p <- ggplot(d,aes(x=factor(d$Index),y=d$Singular_value)) + 
				geom_bar(stat="identity",width=.5,fill="#3399FF") + 
				labs(title=paste("Scree plot of the ", length(singulars), " Singular values of W matrix",sep="")) +
				xlab("Singular value index") +
				ylab("Values") +
				theme_minimal()
	plot(p)
}

