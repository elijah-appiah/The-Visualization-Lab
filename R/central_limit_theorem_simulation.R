##########################################################################
#                                                                        #
#                         CENTRAL LIMIT THEOREM                          #
#                                                                        #
##########################################################################

# Install and Load `ggplot2` Package
install.packages("ggplot2")
library(ggplot2)

# Reproducibility
set.seed(31)

# Simulation
n_sims <- 5000
sample_sizes <- c(1, 5, 10, 15, 20, 30, 100, 1000)

# Simulate sample means
sim_list <- lapply(sample_sizes, function(n) {
  means <- replicate(n_sims, mean(rexp(n, rate = 1)))
  tibble(sample_size = paste0("n = ", n), mean_val = means)
})

# Combine simulation results
sim_df <- bind_rows(sim_list)

# Preserve sample-size order
sim_df$sample_size <- factor(
  sim_df$sample_size,
  levels = paste0("n = ", sample_sizes)
)

# Define a customized theme for the plot
palette_qvd <- c(
  "#264653", "#2A9D8F", "#E9C46A", "#F4A261",
  "#E76F51", "#6D597A", "tomato", "blue")

bg_col   <- "#FBFAF7"
grid_col <- "#E3E1DB"
text_col <- "#2B2B2B"

theme_qvd <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background  = element_rect(fill = bg_col, color = NA),
      panel.background = element_rect(fill = bg_col, color = NA),
      panel.grid.major = element_line(color = grid_col, linewidth = 0.35),
      panel.grid.minor = element_blank(),
      text = element_text(color = text_col),
      plot.title = element_text(face = "bold", size = rel(1.25), margin = margin(b = 8)),
      plot.subtitle = element_text(color = "#555555", size = rel(0.9)),
      plot.caption = element_text(color = "#888888", size = rel(0.65), hjust = 0, margin = margin(t = 10)),
      axis.title = element_text(size = rel(0.85)),
      legend.position = "top",
      legend.title = element_text(face = "bold", size = rel(0.8))
    )
}

palette_qvd <- c(
  "#264653", "#2A9D8F", "#E9C46A", "#F4A261",
  "#E76F51", "#6D597A", "tomato", "blue"
)

# Visualize convergence to normality
ggplot(sim_df, aes(mean_val, fill = sample_size)) +
  geom_histogram(
    bins = 40,
    color = bg_col,
    linewidth = 0.15
  ) +
  scale_fill_manual(values = palette_qvd) +
  facet_wrap(
    ~sample_size,
    scales = "free",
    ncol = 2
  ) +
  labs(
    title = "Central Limit Theorem",
    subtitle = "5,000 Monte Carlo replications per panel; distribution converges to Normal as n grows",
    x = "Sample mean",
    y = "Count",
    caption = "Simulated data: Monte Carlo draws from Exponential(rate=1), demonstrating the Central Limit Theorem."
  ) +
  theme_qvd() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )
