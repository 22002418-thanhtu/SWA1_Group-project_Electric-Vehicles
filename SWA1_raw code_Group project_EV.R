# Question 1

# 1a
# install.packages("RedditExtractoR") (Install necessary library)
# Find top reddit threads URLs from r/electric vehicles in the past year
library(RedditExtractoR)
ev_threads = find_thread_urls(
  subreddit = "electricvehicles",
  sort_by = "top",
  period = "year"
)
# Save the dataset
write.csv(ev_threads, "ev_threads.csv", row.names = FALSE)`
colnames(ev_threads)`
# Get thread content and comment for other questions
urls_extract <- ev_threads$url
thread_data <- get_thread_content(urls_extract)
reddit_threads<- thread_data$threads
reddit_comments<- thread_data$comments

# Save the content and comment
ev_threads <- read.csv("ev_threads.csv")
threads <- read.csv("reddit_threads.csv")
comments <- read.csv("reddit_comments.csv")
ev_threads
threads
comments


#1b
# Install necessary libraries
# install.packages(c("tidytext", "dplyr", "ggplot2", "tidyr"))
# Load necessary libraries
library(dplyr)
library(stringr)
library(kableExtra)
#Sort by descending score
sorted_threads <- threads %>%
  arrange(desc(score))
#Get top 3
final_top_3 <- sorted_threads[1:3, ]
#Format data
report_data <- final_top_3 %>%
  select(score, title, text) %>%
  mutate(
    title = str_wrap(title, width = 40),
    text  = str_wrap(text, width = 60)
  )
#Print table
kable(report_data,
      caption = "Top 3 Reddit Threads by Score",
      col.names = c("Score", "Thread Title", "Main Post Body"),
      booktabs = TRUE,
      longtable = TRUE) %>%
  kable_styling(latex_options = c("hold_position", "striped"))

#1c
library(tidytext)
library(tidyr)
library(ggplot2)
library(dplyr)
library(stringr)
# Remove white space
final_top_3_clean <- final_top_3 %>% mutate(url = trimws(url))
comments_clean <- comments %>% mutate(url = trimws(url))
# Get the list of Thread names in the correct order: Top 1, 2, 3
ordered_titles <- final_top_3_clean %>%
  mutate(title_short = str_trunc(title, 50)) %>%
  pull(title_short) %>%
  unique()
# Filter and combine titles from final_top_3 into comments using inner_join.
top_comments <- comments_clean %>%
  inner_join(final_top_3_clean %>% select(url, title), by = "url") %>%
  mutate(title_short = str_trunc(title, 50)) %>%
  mutate(title_short = factor(title_short, levels = ordered_titles))
# Create Bigrams
bigrams <- top_comments %>%
  unnest_tokens(bigram, comment, token = "ngrams", n = 2)
# Split the bigram into two columns for filtering.
bigrams_separated <- bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
# Download stop_words list
data("stop_words")
# Filter out stop words, NA values, and filter out special numbers/characters
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!is.na(word1), !is.na(word2)) %>%
  filter(str_detect(word1, "^[a-z]+$"), str_detect(word2, "^[a-z]+$")) 
# Combine the two words to form the complete bigram.
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")
# Count frequency
bigram_counts <- bigrams_united %>%
  group_by(title_short, bigram) %>%
  summarise(n = n(), .groups = "drop")
# Get top 15 bigrams for each threads
top_15_bigrams <- bigram_counts %>%
  group_by(title_short) %>%
  slice_max(n, n = 15, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(bigram = reorder_within(bigram, n, title_short))
# Check for empty data and draw a graph.
if(nrow(top_15_bigrams) == 0) {
  print("Error")
} else {
  ggplot(top_15_bigrams, aes(x = n, y = bigram, fill = title_short)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ title_short, scales = "free", ncol = 1) +
    scale_y_reordered() +
    labs(title = "Top 15 Most Frequent Bigrams per Thread",
         x = "Frequency",
         y = "Bigram") +
    theme_minimal() +
    theme(strip.text = element_text(size = 10, face = "bold"),
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
}

# Question 2

#2a
# Load necessary libraries
library(tm)
library(SnowballC)
# Create vector of comments
all_comments <-top_comments$comment
# Create the corpus
reddit.corpus = Corpus(VectorSource(all_comments))
# Standard cleaning steps
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(reddit.corpus, function(x) iconv(x, to='ASCII//TRANSLIT', sub=' '))
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, stripWhitespace)
corpus = tm_map(corpus, removeWords, stopwords()) 
corpus = tm_map(corpus, stemDocument)
# Compute the document term matrix using TF-IDF weighting
reddit.dtm = TermDocumentMatrix(corpus)
reddit.wtdm = weightTfIdf(reddit.dtm)
reddit.matrix = t(as.matrix(reddit.wtdm))
# Remove empty comments
empties <- which(rowSums(abs(reddit.matrix)) == 0)
if(length(empties) > 0) {
  reddit.matrix <- reddit.matrix[-empties,]
}

#2b
# First, normalize all comments to unit length (implements Cosine distance logic)
norm.reddit.matrix = diag(1/sqrt(rowSums(reddit.matrix^2))) %*% reddit.matrix
# Create the distance matrix
D = dist(norm.reddit.matrix, method = "euclidean")^2/2
# Perform MDS using 100 dimensions to reduce noise and improve performance
mds.reddit.matrix <- cmdscale(D, k = 100)
# Initialize variables for the Elbow Method
n = 15
SSW = rep(0, n)
# Run the loop to calculate SSW for each k (1 to 20)
set.seed(123) # make the result consistent
for (a in 1:n) {
  # nstart = 20 helps avoid results based on poor random starting points
  K = kmeans(mds.reddit.matrix, a, nstart = 20)
  SSW[a] = K$tot.withinss
}
# Plot the results to visualize the 'Elbow'
plot(1:15, SSW, type = "b", pch = 19, 
     xlab = "Number of Clusters K", 
     ylab = "Sum of Squared Within-cluster distances (SSW)",
     main = "Elbow Method: Optimal Cluster Analysis")

#2c
library(dplyr)
# Add the Ground Truth labels (A, B, C)
top_comments<- top_comments[as.numeric(rownames(reddit.matrix)), ]
top_comments$ground_truth <- case_when(
  top_comments$url == final_top_3[1, "url"] ~ "A",
  top_comments$url == final_top_3[2, "url"] ~ "B",
  top_comments$url == final_top_3[3, "url"] ~ "C",
)
table(final_report_data$ground_truth)

#2d
# Run final K-means on the 100D MDS matrix (computed in 2b)
# We use 8 clusters as justified by the Elbow Method
set.seed(123) # consistency in results
K_final <- kmeans(mds.reddit.matrix, 8, nstart = 20)
# Attach the cluster assignments to the synced data frame
top_comments$cluster <- as.factor(K_final$cluster)
# Content distribution (Clusters) by Thread (Ground Truth)
clu_gt <- table(top_comments$ground_truth, top_comments$cluster)
clu_gt

#2e
library(ggplot2)
# Reduce to 2 dimensions
mds2.reddit.matrix <- cmdscale(D, k=2)
# Data preparation
plot_df <- data.frame(
  MDS1 = mds2.reddit.matrix[,1],
  MDS2 = mds2.reddit.matrix[,2],
  Cluster = final_report_data$cluster,
  Thread = final_report_data$ground_truth
)
# Plot results
ggplot(plot_df, aes(x = MDS1, y = MDS2, color = Cluster, shape = Thread)) +
  geom_point(size = 3) +
  scale_shape_manual(values = c(1, 2, 3)) +
  theme_minimal() +
  labs(title = "2D vector space: Cluster Visualization", x = "MDS 1", y = "MDS 2")+
  theme(legend.position = "right")

# Question 3

#Create a scatter plot with a linear regression line to see the trend
ggplot(data = threads, aes(x = score, y = comments)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red")
#Perform a linear regression analysis and summarize the model
fit = lm(comments ~ score, data = threads)
summary(fit)

# Question 4

#4a

# install.packages("rtoot")
library(rtoot)
library(dplyr)
# Setup authentication 
#auth_setup(instance = "mastodon.social", type = "user")
# Identify users related to hashtag "electric vehicles"
ev_toots <- get_timeline_hashtag(hashtag = "ElectricVehicles", 
                                 instance = "mastodon.social", 
                                 limit=100)
ev_users <- ev_toots$account
# Combine the list of users and remove duplicated users
ev_users_combined <- do.call(rbind, ev_users)
ev_unique <- ev_users_combined[!duplicated(ev_users_combined$id), ]
# Identify the top three most popular users based on the highest number of Statuses
top_3_active <- ev_unique %>%
  filter(locked == FALSE,) %>%
  arrange(desc(statuses_count)) %>%
  slice(1:3)
# save the data file
saveRDS(top_3_active, "top_3_active.rds")
# Read the data file
top_3_active<-readRDS("top_3_active.rds")
top_3_active

#4b

#Download 50 followers and 50 friends from these users and add them into a list
top_3_active = readRDS("top_3_active.rds")
results <- lapply(top_3_active$id, function(user_id) {
  followers <- get_account_followers(user_id, limit = 50)
  followers <- followers[!duplicated(followers$id), ]
  friends <- get_account_following(user_id, limit = 50)
  friends <- friends[!duplicated(friends$id), ]
  if (nrow(friends) > 50) {friends <- friends[1:50, ]}
  if (nrow(followers) > 50) {followers <- followers[1:50, ]}
  list(
    user_id = user_id,
    followers = followers,
    friends = friends
  )
})
#Create an edge list
el <- do.call(rbind, lapply(seq_along(results), function(i) {
  user_name <- top_3_active$username[i]
  followers <- results[[i]]$followers
  friends   <- results[[i]]$friends
  temp <- NULL
  
  # followers -> user
  if (!is.null(followers) && nrow(followers) > 0) { #prevents missing data to cause errors
    temp <- rbind(temp,
                  cbind(followers$username, user_name))
  }
  
  # user -> friends
  if (!is.null(friends) && nrow(friends) > 0) {
    temp <- rbind(temp,
                  cbind(user_name, friends$username))
  }
  
  return(temp)
}))
colnames(el) <- c("from", "to")
#Create network graph
g_ev <- graph_from_edgelist(as.matrix(el), directed = TRUE)
plot(g_ev,
     layout = layout_with_fr,
     vertex.size = 6,
     vertex.label.cex = 0.7,
     edge.arrow.size = 0.3)

#4c

#---PageRank---
# 1. Create mathematical functions
# Normalise a vector so it sums to 1
normalise = function(x) {
  if(sum(x) != 0) {
    return(x / sum(x))
  } else return(0)
}
# Convert adjacency matrix to transition probability matrix
adjacency.to.probability = function(A) {
  cols = ncol(A)
  for (a in 1:cols) {
    A[, a] = normalise(A[, a])
  }
  return(A)
}
# Calculate Euclidean distance for convergence check
difference = function(x, y) {
  return(sqrt(sum((x - y)^2)))
}
# The Power Method for stationary distribution
stationary.distribution = function(T) {
  n = ncol(T)
  p = rep(0, n)
  p[1] = 1
  p.old = rep(0, n)
  while (difference(p, p.old) > 1e-06) {
    p.old = p
    p = T %*% p.old
  }
  return(p)
}

# Apply the result to the created function
# Obtain the directed graph adjacency and transition matrix
# We transpose it so columns point to rows as per the lab requirements 
A_ev <- t(as.matrix(get.adjacency(g_ev)))
T_ev <- adjacency.to.probability(A_ev)
# Create the random jump matrix J
# n is the total number of nodes in your graph (Hubs + Followers + Friends)
n <- ncol(T_ev)
J <- matrix(rep(1/n, n * n), n, n)
# Combine T and J to obtain matrix M using alpha (damping factor)
alpha <- 0.85 # Standard PageRank alpha
M <- alpha * T_ev + (1 - alpha) * J
# Ensure columns sum to 1
M <- adjacency.to.probability(M)
# Compute the PageRank (Stationary Distribution)
pagerank_scores <- stationary.distribution(M)
# Organize results
pagerank_results <- data.frame(
  Node = V(g_ev)$name,
  PageRank = as.numeric(pagerank_scores)
)
# Sort by highest rank to see the most "important" users
pagerank_results <- pagerank_results[order(-pagerank_results$PageRank), ]
print(head(pagerank_results, 10))

#---Degree Centrality---
# 1. Calculate the Degree for every node in your graph 
# mode = "all" considers both followers and friends
node_degrees <- degree(g_ev, mode = "all")
# 2. Find the most central vertices
# We use order with decreasing = TRUE to rank them
central_nodes_order <- order(node_degrees, decreasing = TRUE)
# 3. Create a ranked data frame of the top 10 most central nodes
degree_results <- data.frame(
  Node = V(g_ev)$name[central_nodes_order],
  Degree = node_degrees[central_nodes_order]
)
# Display the top 10 most central users
print(head(degree_results, 10))

#---Camparison between PageRank and Degree Centrality---
# Merge the two data frames by the Node name
comparison_table <- merge(degree_results, pagerank_results, by = "Node")
# Sort by PageRank to see if the top influencers have the most connections
comparison_table <- comparison_table[order(-comparison_table$PageRank), ]
# Display the top 10 for comparison
print(head(comparison_table, 10))