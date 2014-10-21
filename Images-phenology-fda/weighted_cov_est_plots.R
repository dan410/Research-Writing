############################################################################
### Code to produce plots related to the weighted covariance estimator
############################################################################

library(ggplot2)
library(plyr)


############################################################################
### Plot simulation grid spatial locations
############################################################################

locs <- readRDS("Data/sim_locs.rds")

grid <- subset(locs, grid == 1)

gp <- ggplot(grid, aes(x=x, y=y)) + 
geom_point() + 
theme_bw()+
labs(x = "", y = "")
gp

ggsave("Plots/grid.pdf", height = 4, width = 5)

############################################################################
### Plot spatial correlation curves
############################################################################
library(geoR) # used to simulate gaussian random fields

## first lets look at how covariance functions are specified in geoR
library(geoR)
cov.f <- function(x,...){
		cov.spatial(x, ...)
}

# plot exponential covariance functions
x <- seq(0,1, length = 1000)
exp_r3 <- cov.f(x, cov.model="exponential", cov.pars=c(1,.3))
exp_r3_df <- data.frame(x = x, y = exp_r3, range = 0.3)

x <- seq(0,1, length = 1000)
exp_r2 <- cov.f(x, cov.model="exponential", cov.pars=c(1,.2))
exp_r2_df <- data.frame(x = x, y = exp_r2, range = 0.2)

x <- seq(0,1, length = 1000)
exp_r1 <- cov.f(x, cov.model="exponential", cov.pars=c(1,.1))
exp_r1_df <- data.frame(x = x, y = exp_r1, range = 0.1)

cov_df <- rbind(exp_r1_df, exp_r2_df, exp_r3_df)
cov_df$range <- factor(cov_df$range)

gp <- ggplot(cov_df, aes(x = x, y = y, group=range, linetype = range, color = range)) + 
	geom_line()+
	theme_bw()+
  scale_color_manual(values = c("cornflowerblue", "darkolivegreen3", "firebrick1"))+
	labs(x = "", y = "correlation")+
	guides(color = guide_legend('range'), linetype = guide_legend('range'))
gp

# ggsave("Plots/exp_corr_funs.pdf", width = 8, height = 5)

############################################################################
### Plot results of covariance function estimates for different levels of spatial dependence and different spatial weights
############################################################################

### Read in sim results
all_dat <- readRDS("Data/sim_res_grid.rds")

res_means <- ddply(subset(all_dat, L2 < 0.55), .(dep, weight), summarize, tmean = mean(L2), se = sd(L2)/50, n = 100 )
res_means <- subset(res_means, weight != 4)
res_ind <- subset(res_means, dep == 0.001)
res_means$dep <- factor(res_means$dep, levels = c(0.100, 0.200, 0.300, 0.001), labels=c( "0.1", "0.2", "0.3", "ind"))

# Define the top and bottom of the errorbars
limits <- aes(ymax = tmean + 2*se, ymin = tmean - 2*se)

gp <- ggplot(res_means, aes(x = weight, y = tmean, group = dep, color=dep, linetype = dep))+
geom_line() + geom_errorbar(limits, width=0.05, linetype = 1, color = "black") +
scale_color_manual(values = c( "cornflowerblue", "darkolivegreen3", "firebrick1", "black"))+
xlim(c(-0.05,1.05))+
theme_bw()+
labs(x="weight parameter, p", y = "mean integrated squared error", color = "spatial\ndependence\n(range)", linetype = "spatial\ndependence\n(range)")
gp

# ggsave("Plots/MSE_trends.pdf", width = 8, height = 5)

############################################################################
### 
############################################################################

all_dat <- readRDS("Data/sim_res_india.rds")
	