### Making some toy plots to demonstrate how KS works!
library(ggplot2)
library(ggpubr)

## TOY PLOT 1: 

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
       title = "Sample ECDF vs Normal Reference Distribution ") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))

# Save 
ggsave(filename = "figures/ECDF_Normal_Comp.png", plot = p1, device = "png", height = 6, width = 8, dpi = 1200)



## TOY PLOT 2:

# Define the normal distribution parameters
mu <- 0
sigma <- 1

# Generate x values for the normal distribution
x <- seq(-4, 4, length.out = 1000)

# Calculate the normal density
y <- dnorm(x, mean = mu, sd = sigma)

# Create a data frame for the shaded regions
df <- data.frame(x = x, y = y)

# Shade the tails
p2 <- ggplot(df, aes(x, y)) +
  geom_area(data = subset(df, x < qnorm(0.025)), fill = "blue") +
  geom_area(data = subset(df, x > qnorm(0.975)), fill = "green") +
  geom_line(color = "black") +
  labs(title = "Normal Distribution & Tail Probabilities", 
       x = "", y = "") +
  theme_minimal()+ theme(plot.title = element_text(hjust = 0.5))

# Save
ggsave(filename = "figures/Normal_Tails.png", plot = p2, device = "png", height = 6, width = 8, dpi = 1200)



