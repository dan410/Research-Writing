library(scatterplot3d)
library(ssfcov2)
library(sfdasim)

#################################################
 # curves <- sim_fda_curves(nbasis = 50, nfuns = 50, type="Cos", basis.pars = 2)	
 # sim.data.all <- sim_fda_data(coef = curves$coef, basis.fns = curves$basis.fns, sigma0=0.368, m = 100)
 # save(list=c('curves', 'sim.data.all'), file='cy-alldata.Rda')
  
 # sim.data <- NULL
 # m = 5
 # for( i in 1:length(unique(sim.data.all$ID))){
 	# sim.data <- rbind(sim.data, subset(sim.data.all, ID == i)[1:m,])
 # } 
 # covfit <- estimate_cov_function(sim.data, n.marginal.knots=5)
 # efit <- estimate_eigenfunctions(covfit) 
 # save(list=c('sim.data', 'covfit', 'efit'), file="covfitcy-m5.Rda")
 
 # sim.data <- NULL
 # m = 10
 # for( i in 1:length(unique(sim.data.all$ID))){
 	# sim.data <- rbind(sim.data, subset(sim.data.all, ID == i)[1:m,])
 # } 
 # covfit <- estimate_cov_function(sim.data, n.marginal.knots=5)
 # efit <- estimate_eigenfunctions(covfit) 
 # save(list=c('sim.data', 'covfit', 'efit'), file="covfitcy-m10.Rda")
 
 # sim.data <- NULL
 # m = 40
 # for( i in 1:length(unique(sim.data.all$ID))){
 	# sim.data <- rbind(sim.data, subset(sim.data.all, ID == i)[1:m,])
 # } 
 # covfit <- estimate_cov_function(sim.data, n.marginal.knots=5)
 # efit <- estimate_eigenfunctions(covfit) 
 # save(list=c('sim.data', 'covfit', 'efit'), file="covfitcy-m40.Rda")
 
 load('cy-alldata.Rda')

# plot of simulated curves 
 pdf('../Images-nonparametric/cy-curves.pdf')
 plot_curves(curves$coef, curves$basis.fns, ylim=c(-2,2))
 dev.off()
######### plots for m = 5 observations per curve ##########
 load('covfitcy-m5.Rda')
# line plot of noisy observations
pdf('../Images-nonparametric/cy-data-m5.pdf') 
plot_data(sim.data)
dev.off()

# scatter plot of sample covariance
pdf('../Images-nonparametric/cy-scatter3d-m5.pdf')
scatterplot3d(x=covfit$samp.cov$time1, y=covfit$samp.cov$time2,z=covfit$samp.cov$z, angle=40, zlim= c(-1.5,1.5), xlab="", ylab="", zlab="", main="", pch=20, cex.symbols=0.3)
dev.off()

# plot estimated covariance fuction
zlim = c(-1.5, 1.5)
screen = list(x = -90, y = -15, z= 0) # orientation for wireframe plots
pdf('../Images-nonparametric/cy-fit-image-m5.pdf')
plot_covfit(covfit, image=TRUE, zlim=zlim)
dev.off()
pdf('../Images-nonparametric/cy-unpen-image-m5.pdf')
plot_covfit(covfit, onlyUnpen=TRUE, image=TRUE, zlim=zlim)
dev.off()
pdf('../Images-nonparametric/cy-pen-image-m5.pdf')
plot_covfit(covfit, onlyPen=TRUE, image=TRUE, zlim=zlim)
dev.off()
pdf('../Images-nonparametric/cy-fit-wireframe-m5.pdf')
plot_covfit(covfit, zlim=zlim, screen=screen)
dev.off()
pdf('../Images-nonparametric/cy-unpen-wireframe-m5.pdf')
plot_covfit(covfit, onlyUnpen=TRUE, zlim=zlim, screen=screen)
dev.off()
pdf('../Images-nonparametric/cy-pen-wireframe-m5.pdf')
plot_covfit(covfit, onlyPen=TRUE, zlim=zlim, screen=screen)
dev.off()

# extract first two estimated eigenfunctions
eigfit <- estimate_eigenfunctions(covfit)
ef1.m5 <- eigfit$fns[[1]]
curve(ef1.m5)
ef2.m5 <- eigfit$fns[[2]]
curve(ef2.m5)

######### plots for m = 10 observations per curve ##########
 load('covfitcy-m10.Rda')
# line plot of noisy observations
pdf('../Images-nonparametric/cy-data-m10.pdf') 
plot_data(sim.data)
dev.off()

# scatter plot of sample covariance
pdf('../Images-nonparametric/cy-scatter3d-m10.pdf')
scatterplot3d(x=covfit$samp.cov$time1, y=covfit$samp.cov$time2,z=covfit$samp.cov$z, angle=40, zlim= c(-1.5,1.5), xlab="", ylab="", zlab="", main="", pch=20, cex.symbols=0.3)
dev.off()

# plot estimated covariance fuction
zlim = c(-1.5, 1.5)
screen = list(x = -90, y = -15, z= 0) # orientation for wireframe plots
pdf('../Images-nonparametric/cy-fit-image-m10.pdf')
plot_covfit(covfit, image=TRUE, zlim=zlim)
dev.off()
pdf('../Images-nonparametric/cy-unpen-image-m10.pdf')
plot_covfit(covfit, onlyUnpen=TRUE, image=TRUE, zlim=zlim)
dev.off()
pdf('../Images-nonparametric/cy-pen-image-m10.pdf')
plot_covfit(covfit, onlyPen=TRUE, image=TRUE, zlim=zlim)
dev.off()
pdf('../Images-nonparametric/cy-fit-wireframe-m10.pdf')
plot_covfit(covfit, zlim=zlim, screen=screen)
dev.off()
pdf('../Images-nonparametric/cy-unpen-wireframe-m10.pdf')
plot_covfit(covfit, onlyUnpen=TRUE, zlim=zlim, screen=screen)
dev.off()
pdf('../Images-nonparametric/cy-pen-wireframe-m10.pdf')
plot_covfit(covfit, onlyPen=TRUE, zlim=zlim, screen=screen)
dev.off()

# extract first two estimated eigenfunctions
eigfit <- estimate_eigenfunctions(covfit)
ef1.m10 <- eigfit$fns[[1]]
curve(ef1.m10)
ef2.m10 <- eigfit$fns[[3]]
curve(ef2.m10)

######### plots for m = 40 observations per curve ##########
 load('covfitcy-m40.Rda')
# line plot of noisy observations
pdf('../Images-nonparametric/cy-data-m40.pdf') 
plot_data(sim.data)
dev.off()

# scatter plot of sample covariance
pdf('../Images-nonparametric/cy-scatter3d-m40.pdf')
scatterplot3d(x=covfit$samp.cov$time1, y=covfit$samp.cov$time2,z=covfit$samp.cov$z, angle=40, zlim= c(-1.5,1.5), xlab="", ylab="", zlab="", main="", pch=20, cex.symbols=0.3)
dev.off()

# plot estimated covariance fuction
zlim = c(-1.5, 1.5)
screen = list(x = -90, y = -15, z= 0) # orientation for wireframe plots
pdf('../Images-nonparametric/cy-fit-image-m40.pdf')
plot_covfit(covfit, image=TRUE, zlim=zlim)
dev.off()
pdf('../Images-nonparametric/cy-unpen-image-m40.pdf')
plot_covfit(covfit, onlyUnpen=TRUE, image=TRUE, zlim=zlim)
dev.off()
pdf('../Images-nonparametric/cy-pen-image-m40.pdf')
plot_covfit(covfit, onlyPen=TRUE, image=TRUE, zlim=zlim)
dev.off()
pdf('../Images-nonparametric/cy-fit-wireframe-m40.pdf')
plot_covfit(covfit, zlim=zlim, screen=screen)
dev.off()
pdf('../Images-nonparametric/cy-unpen-wireframe-m40.pdf')
plot_covfit(covfit, onlyUnpen=TRUE, zlim=zlim, screen=screen)
dev.off()
pdf('../Images-nonparametric/cy-pen-wireframe-m40.pdf')
plot_covfit(covfit, onlyPen=TRUE, zlim=zlim, screen=screen)
dev.off()

# extract first two estimated eigenfunctions
eigfit <- estimate_eigenfunctions(covfit)
ef1.m40 <- eigfit$fns[[1]]
curve(ef1.m40)
ef2.m40 <- eigfit$fns[[2]]
curve(ef2.m40)
############################################################

# plot the true covariance funciton used in Cai and Yuan 2010
covf_cy <- function(s,t, alpha=2){
	res <- mapply(eval_covf_cy, s=s, t=t, alpha=alpha)
}
eval_covf_cy <- function(s,t, alpha){
	k <- 1:50
		res <- sum(k^(-1*2*alpha)*cos(k*pi*s)*cos(k*pi*t))	
	return(res)
}

tt <- seq(0,1, length =  40)
grid <- expand.grid(t1 = tt, t2 = tt)
surface <- mapply(covf_cy, s = grid[,1], t=grid[,2])

pdf('../Images-nonparametric/cy-true-image.pdf')  
 z <- matrix(surface, nrow=length(tt), byrow=TRUE)
 x <- tt
 y <- tt
 image.plot(x=x, y=y, z=z, zlim=zlim)
dev.off()
pdf('../Images-nonparametric/cy-true-wireframe.pdf')
wireframe(surface ~ grid[, 1] * grid[, 2], drape = TRUE, 
            pretty = TRUE, scales = list(arrows = FALSE), xlab = "", 
            ylab = "", zlab = "", zlim = zlim, screen=screen)

dev.off()
#######################################################

### plot eigenfunctions ###
norm1 <- integrate(f=function(x){cos(pi*x)^2}, 0,1)$value
norm2 <- integrate(f=function(x){cos(2*pi*x)^2}, 0,1)$value
ef1.true <- function(x){cos(pi*x)/sqrt(norm1)}
ef2.true <- function(x){cos(2*pi*x)/sqrt(norm2)}
curve(ef1.true)
curve(ef2.true)
integrate(f=function(x){ef1.true(x)^2}, 0,1)

 