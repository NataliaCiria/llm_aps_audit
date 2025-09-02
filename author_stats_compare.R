# Load required libraries
library(dplyr)     # Data manipulation
library(tidyr)     # Data reshaping
library(ggplot2)    # Plots creation
library(igraph)    # Network analysis
library(visNetwork) # Interactive network visualization
library(ggrepel)
library(svglite)

# Read in data files
author_stats <- read.csv("data/ground_truth/authors_stats.csv")
author_demographics <- read.csv("data/ground_truth/authors_demographics.csv")
author_pca <- read.csv("data/ground_truth/authors_PCA.csv")
author_all<-left_join(author_stats, author_demographics,by="id_author_oa")
# Read LLM author data and convert string to boolean
llm_author <- read.csv("data/audit/factuality_author.csv")

# Load clean data
top1500_h_index <- read.csv("clean_data/top1500_h_index.csv")
llm_top5 <- read.csv("clean_data/llm_top5.csv")

field_authors <- read.csv("clean_data/field_nodes.csv")
# Fix for the secondary axis scaling issue
density_distribution_h_index_with_count <- author_all %>%
  mutate(in_top5 = id_author_oa %in% llm_top5$id_author_oa) %>%
  ggplot(aes(x = h_index)) +
  # Add outline for entire distribution
  geom_density(color = "white", fill = NA, size = 1.2) +
  # Annotate the highlighted area
  annotate("text", x = mean(c(88, max(author_all$h_index))), 
           y = max(density(author_all$h_index)$y) * 0.7, 
           label = "Top 1500 Authors (h-index ≥ 88)", 
           color = "#00FFFF", size = 4, fontface = "bold") +
  # Add a line at h-index = 88
  geom_vline(xintercept = 88, 
             color = "#00FFFF",
             size = 2,
             alpha = 0.8) +
  # Use vertical lines for top 5
  geom_vline(data = . %>% filter(in_top5 == TRUE), 
             aes(xintercept = h_index), 
             color = "#FF00FF",
             linetype = "dashed",
             size = 1,
             alpha = 0.8) +
  
  # Make sure to use the correct variables and scaling factor
  scale_y_continuous(
    name = "Density",
    sec.axis = sec_axis(~ . * (max(llm_top5$llm_count) / max(density(author_all$h_index)$y)), 
                        name = "LLM Retrieval Count")
  ) +
  
  # Add points for count values with correct scaling
  geom_point(
    data = left_join(
      filter(author_all, id_author_oa %in% llm_top5$id_author_oa),
      llm_top5 %>% select(id_author_oa, llm_count),
      by = "id_author_oa"
    ),
    aes(
      x = h_index,
      # Make sure to use llm_count not count
      y = llm_count * max(density(author_all$h_index)$y) / max(llm_top5$llm_count)
    ),
    color = "#FF00FF",
    size = 3
  ) +
  
  # Fix label placement
  geom_text_repel(
    data = left_join(
      filter(author_all, id_author_oa %in% llm_top5$id_author_oa),
      llm_top5 %>% select(id_author_oa, llm_count),
      by = "id_author_oa"
    ),
    aes(
      x = h_index, 
      y = llm_count * max(density(author_all$h_index)$y) / max(llm_top5$llm_count),
      label = paste0(longest_name, " (", llm_count, ")")
    ),
    color = "white",
    size = 3.5,
    direction = "both",
    force = 2,
    box.padding = 0.5,
    point.padding = 0.5
  ) +
  
  theme_dark() +
  theme(
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black"),
    panel.grid.major = element_line(color = "#333333"),
    panel.grid.minor = element_line(color = "#222222"),
    axis.text = element_text(color = "white"),
    axis.title = element_text(color = "white", face = "bold"),
    plot.title = element_text(color = "white", face = "bold", hjust = 0.5),
    plot.subtitle = element_text(color = "white", hjust = 0.5),
    # Make the secondary axis more visible
    axis.title.y.right = element_text(color = "#FF00FF", face = "bold"),
    axis.text.y.right = element_text(color = "#FF00FF", size = 10)
  ) +
  labs(
    title = "Density Distribution of Authors by h-index with LLM Retrieval Count",
    subtitle = "Cyan area shows top 1500 authors (h-index ≥ 88); magenta points indicate top 5 with retrieval counts",
    x = "h-index"
  ) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.1)))

plot(density_distribution_h_index)
# Save the density distribution plot
ggsave(
  filename = "plots/density_distribution_h_index.svg",
  plot = density_distribution_h_index,
  device = "svg",
  width = 10,
  height = 7,
  dpi = 300
)

# Create a percentage density plot using ggplot2
ggplot() +
  # Add density for field_authors (with normalized area = 1)
  geom_density(data = field_authors, aes(x = h_index, fill = "Field Authors"), 
               alpha = 0.3, color = NA) +
  geom_density(data = field_authors, aes(x = h_index, color = "Field Authors"), 
               fill = NA, size = 1.2) +
  
  # Add density for author_all (with normalized area = 1)
  geom_density(data = author_all, aes(x = h_index, fill = "All Authors"), 
               alpha = 0.3, color = NA) +
  geom_density(data = author_all, aes(x = h_index, color = "All Authors"), 
               fill = NA, size = 1.2) +
  
  # Define colors
  scale_fill_manual(values = c("Field Authors" = "#FF00FF", "All Authors" = "#00FFFF"), 
                    name = "Author Group") +
  scale_color_manual(values = c("Field Authors" = "#FF00FF", "All Authors" = "#00FFFF"), 
                     name = "Author Group") +
  
  # Theme and styling
  theme_dark() +
  theme(
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black"),
    panel.grid.major = element_line(color = "#333333"),
    panel.grid.minor = element_line(color = "#222222"),
    axis.text = element_text(color = "white"),
    axis.title = element_text(color = "white", face = "bold"),
    plot.title = element_text(color = "white", face = "bold", hjust = 0.5),
    plot.subtitle = element_text(color = "white", hjust = 0.5),
    legend.background = element_rect(fill = "black"),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white", face = "bold"),
    legend.key = element_rect(fill = "black")
  ) +
  labs(
    title = "Density Distribution Comparison of h-indices",
    subtitle = "Each distribution normalized to its own population (area under each curve = 1)",
    x = "h-index",
    y = "Density"
  ) +
  coord_cartesian(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2)))
