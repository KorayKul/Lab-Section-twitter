# plot_twitter_network.R
#
# Contributors:
#
# What this file does:
#   * Transforms returned Twitter data into a network of connections
#   * Plots retweet and Mentions networks

# --- Libraries --- #
library(readr)     # read/write files
library(dplyr)     # data manip
library(tidyr)     # data manip
library(tidygraph) # network data
library(ggraph)    # network viz

# --- Load Data --- #
tweets <- read_rds("data/tweets.RDS")

# --- Retweet Network --- #
rt <- tweets %>% 
    filter(is_retweet == TRUE) 

colnames(rt)

rt_edge <- rt %>% 
    select(from = screen_name, to=retweet_screen_name) %>% 
    distinct()

to_counts <- rt_edge %>% 
    group_by(to) %>% 
    count() %>% 
    arrange(desc(n))


from_counts <- rt_edge %>% 
    group_by(from) %>% 
    count() %>% 
    arrange(desc(n)) 

from_filter <- from_counts %>% 
    filter(n >= 3) 

rt_edge_small <- rt_edge %>% filter(from %in% from_filter$from)

rt_graph <- as_tbl_graph(rt_edge_small,directed = FALSE)

ggraph(rt_graph, layout ="fr") +geom_edge_link(alpha = 1, color = "red") + geom_node_point(shape=0,color ="blue") + geom_node_text(aes(label=name),size = 5,repel = TRUE)

rt_graph %>%  mutate(influence = centrality_authority()) %>%  ggraph(layout = 'fr') + geom_edge_link(alpha. = 0.5, color='red') + geom_node_point(aes(size = influence))



# --- Mentions Network --- #

mnt <- tweets %>% 
    select(from =screen_name, to = mentions_screen_name) %>% filter(to !="NA")

mnt <- mnt %>%  unnest_longer(to) %>% 
    distinct()

mnt <- mnt %>% filter(from != to)

to_counts <- mnt %>% group_by(to) %>% count() %>% arrange(desc(n))


from_counts <-  mnt %>% group_by(from) %>% count() %>% arrange(desc(n))
from_filter <- from_counts %>% filter(n >= 3)


mnt_small <- mnt %>%  filter(from %in% from_filter$from)

mnt_graph <- as_tbl_graph(mnt_small)

ggraph(mnt_graph, layout = "fr") + geom_node_point() + geom_edge_link(alpha = 1) + theme_graph()


network_graph <- ggraph(mnt_graph, layout = "fr") + 
    geom_node_point() + 
    geom_edge_link() + 
    geom_node_text(aes(label=name),size = 1,repel = TRUE)


ggsave("network_graph.pdf",network_graph)
















