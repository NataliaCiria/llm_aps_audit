library(dplyr)
library(tidyr)
library(igraph)
library(visNetwork)

author_edges<-read.table("data/ground_truth/coauthorships_edgelist.txt")
author_stats<-read.csv("data/ground_truth/authors_stats.csv")
author_demographics<-read.csv("data/ground_truth/authors_demographics.csv")
author_pca<-read.csv("data/ground_truth/authors_PCA.csv")

llm_author<-read.csv("data/audit/factuality_author.csv")
llm_author$is_in_aps<-ifelse(llm_author$is_in_aps=="True",TRUE,FALSE)

length(unique(llm_author$id_author_oa))
length(unique(llm_author$clean_name))

llm_author_distinct<-llm_author%>%
  select(id_author_oa, clean_name, is_in_aps)%>%
  distinct()

nrow(llm_author_distinct)

sum(!is.na(llm_author_distinct$id_author_oa))

mean(llm_author_distinct$is_in_aps)

### Field ###

field_author<-llm_author%>%
  filter(task_name=="field")

nrow(field_author)
length(unique(field_author$id_author_oa))
length(unique(field_author$clean_name))

field_edges<-author_edges%>%
  filter(V1%in%field_author$id_author_oa|V2%in%field_author$id_author_oa)%>%
  rowwise() %>%
  mutate(from=max(across(V1:V2)),
         to=min(across(V1:V2)),
         V1=NULL,
         V2=NULL)%>%
  distinct()

nrow(unique(field_edges))

field_nodes<-author_demographics%>%
  filter(id_author_oa%in%field_author$id_author_oa)%>%
  left_join(author_stats,by="id_author_oa")%>%
  mutate(id=id_author_oa)%>%
  relocate(id)%>%
  distinct()

nrow(field_nodes)

write.csv(field_edges,"clean_data/field_edges.csv")

visNetwork(nodes=field_nodes, edges=field_edges)

### Top 5 ###
top5_author<-llm_author%>%
  filter(task_param=="top_5")

nrow(top5_author)
length(unique(top5_author$id_author_oa))
length(unique(top5_author$clean_name))

top5_edges<-author_edges%>%
  filter(V1%in%top5_author$id_author_oa|V2%in%top5_author$id_author_oa)

top5_nodes<-author_demographics%>%
  filter(id_author_oa%in%top5_author$id_author_oa)%>%
  left_join(author_stats,by="id_author_oa")%>%
  mutate(id=id_author_oa)%>%
  relocate(id)

unique(author_stats$id_author_oa[author_stats$id_author_oa%in%top5_author$id_author_oa])

nrow(top5_nodes)
top5_nodes$id
write.csv(top5_edges,"clean_data/top5_edges.csv")
unique(top5_edges$from)
unique(top5_edges$to)

names(top5_edges)<-c("from","to")

visNetwork(nodes=top5_nodes, edges=top5_edges)

