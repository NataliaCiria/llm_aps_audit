# Load required libraries
library(dplyr)     # Data manipulation
library(tidyr)     # Data reshaping
library(ggplot2)    # Plots creation
library(igraph)    # Network analysis
library(visNetwork) # Interactive network visualization

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
top5_name_run <- top5_author %>%
  group_by(clean_name,run_id) %>%
  summarise(count=n())%>%
  pivot_wider(
    id_cols = run_id,
    names_from = clean_name,
    values_from = count,
    values_fill = 0
  )%>%
  left_join(top5_author[c("run_id","date","time","model")])%>%
  relocate(run_id,date,time,model)

write.csv(top5_name_run, "clean_data/top5_name_run.csv")

# Create summary dataset with counts and demographic information
top5_id_run <- top5_author %>%
  group_by(id_author_oa,run_id) %>%
  summarise(count=n())%>%
  pivot_wider(
    id_cols = run_id,
    names_from = id_author_oa,
    values_from = count,
    values_fill = 0
  )%>%
  left_join(top5_author[c("run_id","date","time","model")])%>%
  relocate(run_id,date,time,model)

write.csv(top5_id_run, "clean_data/top5_id_run.csv")

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

# The invisibles
top1500_h_index<-author_stats%>%
  arrange(desc(aps_h_index))%>%
  left_join(author_demographics)%>%
  top_n(h_index, n=1500)%>%
  select(id_author_oa,longest_name,works_count,aps_years_of_activity,two_year_mean_citedness,h_index,aps_h_index, ethnicity,gender)

write.csv(top1500_h_index, "clean_data/top1500_h_index.csv")

# The visible (by llm)
llm_top5<-author_stats%>%
  left_join(author_demographics)%>%
  inner_join(top5_unique)%>%
  arrange(desc(count))%>%
  select(id_author_oa,longest_name,llm_count=count,works_count,aps_years_of_activity,two_year_mean_citedness,h_index,aps_h_index, ethnicity,gender)
  
write.csv(llm_top5, "clean_data/llm_top5.csv")

#Those returned by llm who are not in APS
top5_unique$clean_name[is.na(top5_unique$id_author_oa)]

# Save edges to CSV
write.csv(top5_edges, "clean_data/top5_unique.csv")

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

# Save edges to CSV
write.csv(field_edges, "clean_data/field_edges.csv")

# Create nodes for network visualization
field_nodes <- author_demographics %>%
  filter(id_author_oa %in% field_author$id_author_oa) %>%
  left_join(author_stats, by="id_author_oa") %>%
  mutate(id = id_author_oa) %>%  # Create ID column for visNetwork
  relocate(id) %>%  # Move ID to first column
  distinct()  # Remove duplicates

nrow(field_nodes)  # Count nodes

# Save edges to CSV
write.csv(field_nodes, "clean_data/field_nodes.csv")


# Create interactive network visualization
visNetwork(nodes=field_nodes, edges=field_edges)

#### Field network ####
head(author_stats)

