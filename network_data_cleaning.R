# Load required libraries
library(dplyr)     # For data manipulation
library(tidyr)     # For data reshaping
library(igraph)    # For network analysis
library(visNetwork) # For interactive network visualization

# Read in data files
author_edges <- read.table("data/ground_truth/coauthorships_edgelist.txt")
author_stats <- read.csv("data/ground_truth/authors_stats.csv")
author_demographics <- read.csv("data/ground_truth/authors_demographics.csv")
author_pca <- read.csv("data/ground_truth/authors_PCA.csv")

# Read LLM author data and convert string to boolean
llm_author <- read.csv("data/audit/factuality_author.csv")
llm_author$is_in_aps <- ifelse(llm_author$is_in_aps=="True", TRUE, FALSE)

# Check unique authors in dataset
length(unique(llm_author$id_author_oa))
length(unique(llm_author$clean_name))

# Create distinct author dataset
llm_author_distinct <- llm_author %>%
  select(id_author_oa, clean_name, is_in_aps) %>%
  distinct()

# Summary statistics
nrow(llm_author_distinct)  # Number of distinct authors
sum(!is.na(llm_author_distinct$id_author_oa))  # Count of non-NA IDs
mean(llm_author_distinct$is_in_aps)  # Proportion of authors in APS

#### Field Analysis ####

# Filter authors by field task
field_author <- llm_author %>%
  filter(task_name=="field")

# Basic statistics
nrow(field_author)  # Number of field authors
length(unique(field_author$id_author_oa))  # Unique IDs
length(unique(field_author$clean_name))  # Unique names

# Create edges for network visualization
field_edges <- author_edges %>%
  # Filter edges where at least one author is in our field subset
  filter(V1 %in% field_author$id_author_oa | V2 %in% field_author$id_author_oa) %>%
  rowwise() %>%
  # Standardize edge direction (from larger ID to smaller ID)
  mutate(
    from = max(across(V1:V2)),
    to = min(across(V1:V2)),
    V1 = NULL,
    V2 = NULL
  ) %>%
  distinct()  # Remove duplicates

nrow(unique(field_edges))  # Count unique edges

# Create nodes for network visualization
field_nodes <- author_demographics %>%
  filter(id_author_oa %in% field_author$id_author_oa) %>%
  left_join(author_stats, by="id_author_oa") %>%
  mutate(id = id_author_oa) %>%  # Create ID column for visNetwork
  relocate(id) %>%  # Move ID to first column
  distinct()  # Remove duplicates

nrow(field_nodes)  # Count nodes

# Save edges to CSV
write.csv(field_edges, "clean_data/field_edges.csv")

# Create interactive network visualization
visNetwork(nodes=field_nodes, edges=field_edges)

#### Top 5 Analysis####

# Filter authors by top_5 parameter
top5_author <- llm_author %>%
  filter(task_param=="top_5")

# Basic statistics
nrow(top5_author)  # Number of top5 authors
length(unique(top5_author$id_author_oa))  # Unique IDs
length(unique(top5_author$clean_name))  # Unique names

# Create edges for network
top5_edges <- author_edges %>%
  filter(V1 %in% top5_author$id_author_oa | V2 %in% top5_author$id_author_oa)

# Create summary dataset with counts and demographic information
top5_unique <- top5_author %>%
  group_by(clean_name, id_author_oa) %>%
  summarise(count=n()) %>%
  left_join(author_demographics, by="id_author_oa") %>%
  left_join(author_stats, by="id_author_oa") %>%
  mutate(id = id_author_oa) %>%
  relocate(id) %>%
  arrange(desc(count))  # Sort by frequency

# Display selected columns
top5_unique[c("clean_name", "id_author_oa", "count")]

# Save edges to CSV
write.csv(top5_edges, "clean_data/top5_unique.csv")