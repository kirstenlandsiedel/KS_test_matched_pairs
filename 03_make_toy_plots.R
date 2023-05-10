### Making some toy plots to demonstrate how KS works!
library(ggplot2)
library(ggpubr)
library(sn) #skewed normal

## TOY PLOT 1: One-Sample KS (ECDF vs Normal Reference Dist)

# Generate random data
set.seed(123)
data <- rt(30, df=29)

# Create a data frame with the empirical and normal CDF values
df <- data.frame(x = sort(data), 
                 ecdf = ecdf(data)(sort(data)),
                 norm_cdf = pnorm(sort(data), mean = mean(data), sd = sd(data)))

# Plot the ECDF and normal CDF
p1 <- ggplot(df, aes(x)) +
  geom_step(aes(y = ecdf), color = "blue", size = 1.2) +
  geom_line(aes(y = norm_cdf), color = "red", size = 1.2) +
  labs(x = "x", y="",
       title = " ") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=14))
p1

# Save 
ggsave(filename = "figures/ECDF_Normal_Comp.png", plot = p1, device = "png", height = 6, width = 8, dpi = 1200)



## TOY PLOT 2: Two-Sample KS (ECDF vs ECDF)

# Generate the two samples
set.seed(123)
t_sample <- rt(n = 30, df = 5)
norm_sample <- rnorm(n = 30)

# Create a data frame of the two samples
df <- data.frame(x = c(t_sample, norm_sample),
                 group = c(rep("t distribution", 30), rep("normal distribution", 30)))

# Calculate the ECDFs using ecdf()
ecdf_t <- ecdf(t_sample)
ecdf_norm <- ecdf(norm_sample)

# Create a data frame of the ECDFs
df_ecdf <- data.frame(x = seq(min(df$x), max(df$x), length.out = 100),
                      y_t = ecdf_t(seq(min(t_sample), max(t_sample), length.out = 100)),
                      y_norm = ecdf_norm(seq(min(norm_sample), max(norm_sample), length.out = 100)))

# Reshape the data into long format
df_ecdf_long <- tidyr::gather(df_ecdf, key = "group", value = "y", y_t, y_norm)

# Create the plot
p2 <- ggplot(data = df_ecdf_long, aes(x = x, y = y, color = group)) +
  geom_step() +
  scale_color_manual(values = c("red", "blue")) +
  labs(x = "x", y = "", color = "Distribution") +
  theme_minimal()+ theme(text=element_text(size=14)) + 
  theme(legend.position = "none")
p2

# Save
ggsave(filename = "figures/Two_sample_KS.png", plot = p2, device = "png", height = 6, width = 8, dpi = 1200)




### TOY PLOT 3: Dist of taui_hats

library(ggplot2)

# Generate data
set.seed(123)
data <- c(rnorm(900), rnorm(50, mean = 3), rnorm(50, mean = -3))

# Calculate density estimate
dens <- density(data, n = 1000)

# Define cutoffs for shading tails
cutoff_left <- quantile(data, 0.05)
cutoff_right <- quantile(data, 0.95)

# Create plot
p3 <- ggplot() +
  # Add density curve
  geom_line(data = data.frame(x = dens$x, y = dens$y), aes(x = x, y = y)) +
  # Add shading to left tail
  geom_area(data = subset(data.frame(x = dens$x, y = dens$y), x < cutoff_left), aes(x = x, y = y), fill = "blue", alpha = 0.3) +
  # Add shading to right tail
  geom_area(data = subset(data.frame(x = dens$x, y = dens$y), x > cutoff_right), aes(x = x, y = y), fill = "red", alpha = 0.3) +
  # Add vertical lines for cutoffs
  geom_vline(xintercept = cutoff_left, linetype = "dashed", color = "blue") +
  geom_vline(xintercept = cutoff_right, linetype = "dashed", color = "red") +
  # Set plot limits
  xlim(min(data), max(data)) +
  ylim(0, max(dens$y) * 1.1) +
  # Add plot labels and title
  labs(x = "", y = "", title = "")+
  theme(plot.title = element_text(hjust = 0.5)) + theme_minimal() +
  theme(text=element_text(size=14))
p3


# Save
ggsave(filename = "figures/Dist_taui_hats.png", plot = p3, device = "png", height = 6, width = 8, dpi = 1200)


### TOY PLOT 4: F(x) versus 1-F(-x) for Symmetric Dist

# Set random seed for reproducibility
set.seed(13)

# Generate data from a symmetric standard normal distribution
data <- rnorm(100)

# Calculate the ECDF of the data
ecdf_data <- ecdf(data)

# Create a data frame for plotting the ECDF and its complement
plot_data <- data.frame(x = seq(min(data), max(data), length.out = 100))
plot_data$ecdf <- ecdf_data(plot_data$x)
plot_data$comp_ecdf <- 1 - ecdf_data(-plot_data$x)

# Plot the ECDF and its complement on the same plot
p4 <- ggplot(plot_data, aes(x = x)) +
  geom_line(aes(y = ecdf, color = "ECDF of data")) +
  geom_line(aes(y = comp_ecdf, color = "Complement of ECDF of neg. data")) +
  xlab("x") +
  ylab("")  + theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal()+ theme(legend.position = "none")+ theme(text=element_text(size=14))
p4

# Save
ggsave(filename = "figures/ecdfs_symmetric.png", plot = p4, device = "png", height = 6, width = 8, dpi = 1200)



### TOY PLOT 5: F(x) versus 1-F(-x) for Non-Symmetric Dist

set.seed(751)
# Generate data from a non-symmetric distribution
data <- rsn(n=50, alpha=3)

# Calculate the ECDF of the data
ecdf_data <- ecdf(data)

# Create a data frame for plotting the ECDF and its complement
max_val <- max(abs(data))
plot_data <- data.frame(x = seq(-max_val, max_val, length.out = 100))
plot_data$ecdf <- ecdf_data(plot_data$x)
plot_data$comp_ecdf <- 1 - ecdf_data(-plot_data$x)

# Plot the ECDF and its complement on the same plot
p5 <- ggplot(plot_data, aes(x = x)) +
  geom_line(aes(y = ecdf, color = "ECDF of data")) +
  geom_line(aes(y = comp_ecdf, color = "Complement of ECDF of neg. data")) +
  xlab("x") +
  ylab("") +
  theme(plot.title = element_text(hjust = 0.5)) + theme_minimal()+
  theme(legend.position = "none") + theme(text=element_text(size=14))
p5


# Save
ggsave(filename = "figures/ecdfs_asymmetric.png", plot = p5, device = "png", height = 6, width = 8, dpi = 1200)







