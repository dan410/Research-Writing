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

# ggsave("Plots/grid.pdf", height = 4, width = 5)

locs$grid <- factor(locs$grid, levels = c('3', '1', '2'), labels = c('low clustering', 'moderate clustering', 'strong clustering'))

ggplot(locs, aes(x = x, y = y)) + 
  geom_point() + 
  facet_wrap(~grid) +
  theme_bw()+
  theme(strip.text.x = element_text(size = 14))+
  theme(panel.margin = unit(2,'lines'))+
  labs(x = "", y = "") 
# ggsave("Plots/grids.pdf", height = 5, width = 12)

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
exp_r4 <- cov.f(x, cov.model="exponential", cov.pars=c(1,.4))
exp_r4_df <- data.frame(x = x, y = exp_r4, range = 0.4)

x <- seq(0,1, length = 1000)
exp_r3 <- cov.f(x, cov.model="exponential", cov.pars=c(1,.3))
exp_r3_df <- data.frame(x = x, y = exp_r3, range = 0.3)

x <- seq(0,1, length = 1000)
exp_r2 <- cov.f(x, cov.model="exponential", cov.pars=c(1,.2))
exp_r2_df <- data.frame(x = x, y = exp_r2, range = 0.2)

x <- seq(0,1, length = 1000)
exp_r1 <- cov.f(x, cov.model="exponential", cov.pars=c(1,.1))
exp_r1_df <- data.frame(x = x, y = exp_r1, range = 0.1)

x <- seq(0.001,1, length = 1000)
exp_r0 <- cov.f(x, cov.model="exponential", cov.pars=c(1,0))
exp_r0_df <- data.frame(x = x, y = exp_r0, range = 0.0)


cov_df <- rbind(exp_r0_df, exp_r1_df, exp_r2_df, exp_r3_df, exp_r4_df)
cov_df$range <- factor(cov_df$range)

gp <- ggplot(cov_df, aes(x = x, y = y, group=range, linetype = range, color = range)) + 
	geom_line()+
	theme_bw()+
  scale_color_manual(values = c('purple', "cornflowerblue", "darkolivegreen3", "firebrick1", 'black'))+
  theme(strip.text.x = element_text(size = 14),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14))+
	labs(x = "distance", y = "correlation")+
	guides(color = guide_legend('Range'), linetype = guide_legend('Range'))
gp

# ggsave("Plots/exp_corr_funs.pdf", width = 8, height = 5)

############################################################################
### Plot results of covariance function estimates for different levels of 
### spatial dependence and different spatial weights (old)
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
### Plot results of covariance function estimates for different levels of 
### spatial dependence and different spatial weights
############################################################################
grid_id <- 1
grid1 <- readRDS(paste('Data/grid', grid_id, '_res.rds', sep = ''))
grid1$grid <- grid_id
grid_id <- 2
grid2 <- readRDS(paste('Data/grid', grid_id, '_res.rds', sep = ''))
grid2$grid <- grid_id
grid_id <- 3
grid3 <- readRDS(paste('Data/grid', grid_id, '_res.rds', sep = ''))
grid3$grid <- grid_id

grid <- rbind(grid1, grid2, grid3)

grid$grid <- factor(grid$grid, levels = c('3', '1', '2'), labels = c('low clustering', 'moderate clustering', 'strong clustering'))

grid$range <- factor(grid$range)

L2_stats <- ddply(grid, .(grid, range, weight), summarize, mean = mean(L2), med = median(L2), se = sd(L2)/sqrt(200))

ggplot(L2_stats, aes(x = weight, y = mean, color = range, linetype = range)) +
  geom_line()+ 
  geom_errorbar(aes(ymax = mean + se, ymin = mean - se), width = 0.01, linetype = 1)+
  scale_color_manual(values = c('purple', "cornflowerblue", "darkolivegreen3", "firebrick1", 'black'))+
  facet_wrap(~grid)+
  theme_bw()+
  theme(strip.text.x = element_text(size = 14),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14))+
  labs(x = 'weight parameter, p', y = "L")+
  guides(color = guide_legend('Range'), linetype = guide_legend('Range'))

# ggsave(file.path('Plots', 'MSE_trends_all.pdf'), width = 12, height = 5)


############################################################################
### 
############################################################################

all_dat <- readRDS("Data/sim_res_india.rds")
	