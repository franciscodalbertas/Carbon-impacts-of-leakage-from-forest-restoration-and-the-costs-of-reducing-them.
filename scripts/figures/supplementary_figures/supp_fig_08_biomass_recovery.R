# Load necessary packages
library(ggplot2)
library(nlme)  # for nonlinear regression
library(minpack.lm)  # For constrained nls fitting
# Read the dataset
df <- read.csv("input_tables/poorter_plot_improved.csv",header = F)

# Rename columns for clarity
colnames(df) <- c("years", "biomass_recovery")

# ---- 2️⃣ Solve for Asymptotic Model Parameters ---- #

# The asymptotic function:
# biomass_recovery = (a * years) / (b + years)
# We want: biomass_recovery = 90 when years = 66
# Solve for `b`:
#   90 = (a * 66) / (b + 66)
#   b = (a * 66 / 90) - 66

# Define asymptotic model with a constraint ensuring f(66) = 90
asymptotic_fixed <- nlsLM(
  biomass_recovery ~ (a * years) / ((a * 66 / 90) - 66 + years),  # Replace `b`
  data = df, start = list(a = 100)
)

# the asymptotic is the only one that goes from 0 to 100%
# build a data set for a theoretical curve from 0 -100 years

new_data <- data.frame(years = seq(0,100,1))

new_data$recovery <- predict(asymptotic_fixed, newdata = new_data)

# Predict values at x = 50 and x = 66
y_50 <- predict(asymptotic_fixed, newdata = data.frame(years = 50))
y_66 <- predict(asymptotic_fixed, newdata = data.frame(years = 66))


recovery_plot <- ggplot(new_data, aes(x = years, y =recovery)) +
  #geom_point(data = df,aes(x = years, y =biomass_recovery),color = "blue", alpha = 0.6) +  # Observed data points
  geom_line(color = "darkgray", size = 1, linetype = "solid") +
  
  # Add vertical dotted lines at x = 50 and x = 66
  geom_vline(xintercept = 50, linetype = "dotted", color = "black") +
  geom_vline(xintercept = 66, linetype = "dotted", color = "black") +
  
  # Add horizontal dotted lines at corresponding y-values
  geom_hline(yintercept = y_50, linetype = "dotted", color = "black") +
  geom_hline(yintercept = y_66, linetype = "dotted", color = "black") +
  
  # Add text labels for the values at (50, y_50) and (66, y_66)
  # geom_text(aes(x = 50, y = y_50, label = paste0("y = ", round(y_50, 1))), vjust = -1, hjust = -0.2) +
  # geom_text(aes(x = 66, y = y_66, label = paste0("y = ", round(y_66, 1))), vjust = -1, hjust = -0.2) +
  # # Formatting
  theme_classic() +
  labs(x = "Years", y = "Above-Ground Biomass Recovery (%)") +
  theme(legend.position = "bottom")+
  scale_x_continuous(breaks = c(0,25,50,66,75,100))+
  scale_y_continuous(breaks = c(0,25,50,75,86.9,90))+
  theme(text = element_text(size = 7))

ggsave("figures/supp_figures/supp_fig_08.png",
       recovery_plot,
       width = 10,
       height = 10,
       units = "cm")

