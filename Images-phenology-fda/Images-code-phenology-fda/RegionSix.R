
library(plyr)
library(reshape)
library(ssfcov2)
load('RegionSix_TestData.Rdata') #testdat


timepts <- 3:49
timepts <- 1:60
m <- length(timepts)
Chl.short <- llply(Chl.alltimes, .fun = function(x){x[timepts]})



dat <- melt(Chl.short)
dat$LC <- rep(LC.v.six, each = length(timepts))

dat$LC <- factor(dat$LC, levels = c(1,5,8,20),
labels = c("Trp Evergreen", "Trp Semievergreen", "Trp Moist Decid", "Coastal Veg"))

time <- (timepts-min(timepts))/(max(timepts) - min(timepts))
dat$time <- rep(time, length(unique(dat$L1)))
dat$locx <- rep(xs,each = m)
dat$locy <- rep(ys,each = m)

head(dat, 20)
names(dat) <- c("obsX", "ID", "LC", "Time", 'locx', 'locy')

dat2 <- dat
#dat2 <- subset(dat, LC ==20 )

pdf('../Images-future-work/region6-vegetation-map.pdf')
gp <- ggplot(dat2, aes(x=locx, y=locy, label=LC))
gp + geom_raster(aes(fill=LC))+theme_bw()
dev.off()
pdf('../Images-future-work/region6-vegetation-map-nolegend.pdf')
gp <- ggplot(dat2, aes(x=locx, y=locy, label=LC))
gp + geom_raster(aes(fill=LC))+theme_bw()+theme(legend.position='none')
dev.off()

pdf('../Images-future-work/region6-curves-nolegend.pdf')
gp <- ggplot(dat2, aes(x=Time, y=obsX, group=ID))
gp <- gp + geom_line(aes(color = LC))
#gp <- gp + geom_smooth(aes(color = LC), alpha=0.0)
gp <- gp + theme_bw() + xlab('t') + ylab('Y(t)')
gp <- gp + theme(legend.position="none", legend.direction="horizontal")
gp
dev.off()

pdf('../Images-future-work/region6-curves-facet.pdf')
gp <- ggplot(dat2, aes(x=Time, y=obsX, group=ID))
gp <- gp + geom_line(aes(color = LC))
#gp <- gp + geom_smooth(aes(color = LC), alpha=0.0)
gp <- gp + theme_bw() + xlab('t') + ylab('Y(t)') + ylim(c(1.5,4))
gp <- gp + theme(legend.position="none", legend.direction="horizontal")
gp <- gp + facet_grid(LC~.)
gp
dev.off()

# center functions around zero
meanX <- mean(dat2$obsX)
dat2$X <- dat2$obsX - meanX

# # fit <- smooth.spline(dat2$Time, dat2$obsX, nknots=8)
# fitX <- dat2$obsX - fitted(fit)
# plot(dat2$Time, fitted(fit), type="l")
pdf('../Images-future-work/region6-curves-centered.pdf')
gp <- ggplot(dat2, aes(x=Time, y=X, group=ID))
gp <- gp + geom_line(aes(color = LC))
#gp <- gp + geom_smooth(aes(color = LC), alpha=0.0)
gp <- gp + theme_bw() + xlab('t') + ylab('Y(t)')+ ylim(c(-1.5,1.5))
gp <- gp + theme(legend.position="top", legend.direction="horizontal")
gp
dev.off()


# fit covariance function
covfit <- estimate_cov_function(dat2, n.marginal.knots=7)
# save(covfit, file='../Images-code-future-work/covfit-region6-knots7.Rda')
load('../Images-code-future-work/covfit-region6-knots7.Rda')

pdf('../Images-future-work/region6-covfit.pdf')
zlim = c(-.4, .4)
plot_covfit(covfit, zlim=zlim)
dev.off()

eig.fit <- estimate_eigenfunctions(covfit)
eig.fns <- eig.fit$fns

ef0 <- function(x){rep(1, length=length(x))}
ef1 <- eig.fns[[1]]
ef2 <- eig.fns[[2]]
ef3 <- eig.fns[[4]]
ef4 <- eig.fns[[7]]
ef5 <- eig.fns[[8]]

ef11 <- function(x){-1*ef1(x)}
ef12p <- function(x){-1*ef1(x) + 0.4*ef2(x)}
ef12n <- function(x){-1*ef1(x) - 0.4*ef2(x)}
ef13p <- function(x){-1*ef1(x) + (0.5)^2*ef3(x)}
ef13n <- function(x){-1*ef1(x) - (0.5)^2*ef3(x)}
ef14p <- function(x){-1*ef1(x) + (0.5)^3*ef4(x)}
ef14n <- function(x){-1*ef1(x) - (0.5)^3*ef4(x)}
ef15p <- function(x){-1*ef1(x) + (0.5)^4*ef5(x)}
ef15n <- function(x){-1*ef1(x) - (0.5)^4*ef5(x)}

ylim=c(-3,3)
t <- 1:50/50
pdf('../Images-future-work/ef0.pdf')
plot(t, ef0(t), ylim=ylim, ylab='', type='l')
dev.off()
pdf('../Images-future-work/ef1.pdf')
plot(t, ef11(t), ylim=ylim, ylab='', type='l')
dev.off()
pdf('../Images-future-work/ef2.pdf')
plot(t, ef2(t), ylim=ylim, ylab='', type='l')
dev.off()
pdf('../Images-future-work/ef3.pdf')
plot(t, ef3(t), ylim=ylim, ylab='', type='l')
dev.off()
pdf('../Images-future-work/ef4.pdf')
plot(t, ef4(t), ylim=ylim, ylab='', type='l')
dev.off()
pdf('../Images-future-work/ef5.pdf')
plot(t, ef5(t), ylim=ylim, ylab='', type='l')
dev.off()

ylim=c(-3,3)
t <- 1:40/40
pdf('../Images-future-work/ef2pn.pdf')
plot(t, ef11(t), ylim=ylim, ylab='', type='l')
points(t, ef11(t) + 0.4*ef2(t), ylim=ylim, ylab='', pch="+", cex=1.5)
points(t, ef11(t) - 0.4*ef2(t), ylim=ylim, ylab='', pch="-",cex=1.5)
dev.off()
pdf('../Images-future-work/ef2pn.pdf')
plot(t, ef11(t), ylim=ylim, ylab='', type='l')
points(t, ef11(t) + 0.4*ef2(t), ylim=ylim, ylab='', pch="+", cex=1.5)
points(t, ef11(t) - 0.4*ef2(t), ylim=ylim, ylab='', pch="-",cex=1.5)
dev.off()
pdf('../Images-future-work/ef3pn.pdf')
plot(t, ef11(t), ylim=ylim, ylab='', type='l')
points(t, ef11(t) + 0.4*ef3(t), ylim=ylim, ylab='', pch="+",cex=1.5)
points(t, ef11(t) - 0.4*ef3(t), ylim=ylim, ylab='', pch="-",cex=1.5)
dev.off()
pdf('../Images-future-work/ef4pn.pdf')
plot(t, ef11(t), ylim=ylim, ylab='', type='l')
points(t, ef11(t) + 0.4*ef4(t), ylim=ylim, ylab='', pch="+",cex=1.5)
points(t, ef11(t) - 0.4*ef4(t), ylim=ylim, ylab='', pch="-",cex=1.5)
dev.off()
pdf('../Images-future-work/ef5pn.pdf')
plot(t, ef11(t), ylim=ylim, ylab='', type='l')
points(t, ef11(t) + 0.4*ef5(t), ylim=ylim, ylab='', pch="+",cex=1.5)
points(t, ef11(t) - 0.4*ef5(t), ylim=ylim, ylab='', pch="-",cex=1.5)
dev.off()



# use spatialfda package to project data onto the basis functions

# source spatial fda functions
sourceDir( "/Users/dan410/Google_Drive/Research/R_packages/spatialfda/R") 

funlist <- list(ef0, ef1, ef2, ef3, ef4, ef5)
emp.basis <- create.empirical.basis(basisfuns = funlist, rangeval=c(0,1))

pdf('../Images-future-work/region6-empbasis.pdf')
plot(emp.basis)
dev.off()

nfuns <- length(unique(dat2$ID))
y <- matrix(dat2$X, ncol = nfuns, byrow = FALSE)
argvals <- dat2[dat2$ID == 1, ]$Time

# use the plyr package to extract the locations
locs <- ddply(dat2, .(ID), function(x){x[1, c("locx", "locy")]})
locs <- locs[,-1]

fit.emp <- smooth.basis(argvals = argvals, y = y, fdParobj = emp.basis)$fd
plot(fit.emp, ylim=c(-2,2))

ev <- eval.fd(evalarg = time, fdobj = fit.emp, returnMatrix=TRUE)


for(id in 1:100){
loc <- subset(dat2, dat2$ID == id)
plot(1,1, xlim=c(0,1), ylim = c(-1,1), type='n')
points(time, loc$X, type="l", col='black')
smooth <- ev[,id]
points(time, smooth, type='l', col="red")
print(id)
Sys.sleep(.8)
}
dat2$pred <- melt(ev)$value
dat2$pred <- dat2$pred + meanX

pdf('../Images-future-work/region6-estimated-curves.pdf')
gp <- ggplot(dat2, aes(x=Time, y=pred, group=ID))
gp <- gp + geom_line(aes(color = LC))
#gp <- gp + geom_smooth(aes(color = LC), alpha=0.0)
gp <- gp + theme_bw() + xlab('t') + ylab('Estimated') + ylim(c(1.5,4))
gp <- gp + theme(legend.position="none", legend.direction="horizontal")
gp
dev.off()

pdf('../Images-future-work/region6-estimated-curves-facet.pdf')
gp <- ggplot(dat2, aes(x=Time, y=pred, group=ID))
gp <- gp + geom_line(aes(color = LC))
#gp <- gp + geom_smooth(aes(color = LC), alpha=0.0)
gp <- gp + theme_bw() + xlab('t') + ylab('Estimated') + ylim(c(1.5,4))
gp <- gp + theme(legend.position="none", legend.direction="horizontal")
gp <- gp + facet_grid(LC~.)
gp
dev.off()

# let's take a look at the coefficients
coefs.all <- fit.emp$coefs
coefs.all <- melt(coefs.all)
coefs.all$ID <- rep(1:100, each=6)
coefs.all$locx <- rep(xs, each=6)
coefs.all$locy <- rep(ys, each=6)
coefs.all$X2 <- NULL
LC <- NULL
for(i in 1:nrow(coefs.all)){
	LC[i] <- unique(subset(dat2, ID == coefs.all$ID[i])$LC)
}
coefs.all$LC <- LC
names(coefs.all) <- c('efn', 'value', 'ID', "LC", 'locx','locy')

coefs1 <- subset(coefs.all, efn==1)
coefs2 <- subset(coefs.all, efn==2)
coefs3 <- subset(coefs.all, efn==3)
coefs4 <- subset(coefs.all, efn==4)
coefs5 <- subset(coefs.all, efn==5)
coefs6 <- subset(coefs.all, efn==6)

jpg('../Images-future-work/coef1-spatial.jpg')
 qplot(locx, locy, data = coefs1, fill = value, geom = "raster", theme=theme_bw)
dev.off()
png('../Images-future-work/coef2-spatial.png')
 qplot(locx, locy, data = coefs2, fill = value, geom = "raster")
dev.off()
pdf('../Images-future-work/coef3-spatial.pdf')
 qplot(locx, locy, data = coefs3, fill = value, geom = "raster")
dev.off()
pdf('../Images-future-work/coef4-spatial.pdf')
 qplot(locx, locy, data = coefs4, fill = value, geom = "raster")
dev.off()
pdf('../Images-future-work/coef5-spatial.pdf')
 qplot(locx, locy, data = coefs5, fill = value, geom = "raster")
dev.off()
pdf('../Images-future-work/coef6-spatial.pdf')
 qplot(locx, locy, data = coefs6, fill = value, geom = "raster")
dev.off()

df <- data.frame(coef1 = coefs1$value, coef2 = coefs2$value, coef3=coefs3$value, coef4 = coefs4$value, coef5 = coefs5$value, coef6 = coefs6$value, LC = coefs1$LC )

pdf('../Images-future-work/boxplot-coefs.pdf')
boxplot(df[,-7], cex.axis=1.4)
dev.off()

df$LC <- factor(df$LC, levels = c(1,2,3,4),
labels = c("Trp Evergreen", "Trp Semievergreen", "Trp Moist Decid", "Coastal Veg"))

par(mfrow=c(2,3))
xlim=c(-0.5,0.5)
hist(df[df$LC!='Trp Semievergreen', ]$coef1, col='red', main='', xlab='',ylab='')
hist(df[df$LC!='Trp Semievergreen', ]$coef2, xlim=xlim, main='', xlab='',ylab='')
hist(df[df$LC!='Trp Semievergreen', ]$coef3, xlim=xlim, main='', xlab='',ylab='')
hist(df[df$LC!='Trp Semievergreen', ]$coef4, xlim=xlim, main='', xlab='',ylab='')
hist(df[df$LC!='Trp Semievergreen', ]$coef5, xlim=xlim, main='', xlab='',ylab='')
hist(df[df$LC!='Trp Semievergreen', ]$coef6, xlim=xlim, main='', xlab='',ylab='')

pdf('../Images-future-work/hist-trpEvgrn-CstVeg-coef3.pdf')
par(mfrow=c(1,2), cex.axis=1.4)
xlim= c(-0.6, 0.5)
hist(df[df$LC %in% c('Trp Evergreen'), ]$coef3, main='Tropical Evergreen', xlab='',ylab='', xlim=xlim, prob=TRUE)
hist(df[df$LC %in% c( 'Coastal Veg'), ]$coef3, main='Coastal Vegetation', xlab='',ylab='', xlim=xlim, prob=TRUE)
dev.off()


pdf('../Images-future-work/region6-matrixplot.pdf')
pairs(df[,2:4], col=df$LC)
dev.off()

df2 <- subset(df, LC != 'Trp Semievergreen')
#pdf('../Images-future-work/region6-matrixplot2.pdf')
pairs(df2[,2:4], col=df$LC)
#dev.off()





############################## old work ################

dat$fit <- melt(ev)$value
dat$mean <- rep(meanfun, 100)
coefs <- fit.emp$coef

fit.all <- dat
coefs.all <- coefs

save(list = c('fit.all', 'coefs.all'), file="fitall.Rdata")

#################################

plot(time, meanfun, type="l", col="black", ylim=c(1.5,3.5))

ev <- eval.fd(evalarg = time, fdobj = fit, returnMatrix=TRUE)
ev2 <- eval.fd(evalarg = time, fdobj = fit.spsmooth, returnMatrix=TRUE)

locID <- 13
loc <- subset(dat, dat$ID == locID)
points(time, loc$obsX, type="l", col='gray')
smooth <- meanfun + ev[,locID]
points(time, smooth, type='l', col="red", lty=2)
spsmooth <- meanfun + ev2[,locID]
points(time, spsmooth, type='l', col="blue", lty=3)


FITS <- dat
FITS$fit <- melt(ev)$value
FITS$fit.spat <- melt(ev2)$value
FITS$mean <- rep(meanfun, 100)


save(FITS, file="FITS.Rdata")

loc <- subset(FITS, ID==30)
with(loc, 
 plot(Time, obsX, type="l", col='gray')
 points(Time, mean+fit, type="l", lty=2, col='red'))
 points(Time, mean+fit.spat, type="l", lty=3, col='blue'))
 points(Time, mean, type="l")
)











