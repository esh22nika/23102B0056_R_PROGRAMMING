# ============================================================================
# Social Network Analysis with R | Examples
# Based on: https://www.youtube.com/watch?v=0xsM0MbRPGE
# ============================================================================

# Load required package
library(igraph)

# -----------------------------------------------------------
# Part 1: Creating a Simple Undirected Graph
# -----------------------------------------------------------
# Create an undirected graph with edges 1-2, 2-3, 3-4, 4-1 and 7 nodes
g <- graph(c(1,2, 2,3, 3,4, 4,1),
           directed = F,
           n = 7)

plot(g,
     vertex.color = "steelblue",
     vertex.size = 25,
     main = "Simple Undirected Graph (7 Nodes)")

# -----------------------------------------------------------
# Part 2: Creating a Directed Graph with Named Nodes
# -----------------------------------------------------------
g1 <- graph(c("Amy", "Ram", "Ram", "Li", "Li", "Amy",
              "Amy", "Li", "Kate", "Li"),
            directed = T)

plot(g1,
     vertex.color = "orange",
     vertex.size = 30,
     edge.arrow.size = 0.5,
     main = "Directed Graph – Named Nodes")

# -----------------------------------------------------------
# Part 3: Network Measures on the Directed Graph
# -----------------------------------------------------------

# Degree centrality (all, in, out)
cat("===== Degree Centrality =====\n")
cat("All:\n")
print(degree(g1, mode = 'all'))

cat("\nIn-degree:\n")
print(degree(g1, mode = 'in'))

cat("\nOut-degree:\n")
print(degree(g1, mode = 'out'))

# Diameter
cat("\nDiameter:", diameter(g1, directed = F, weights = NA), "\n")

# Edge density
cat("Edge Density:", edge_density(g1, loops = F), "\n")

# Manual edge density calculation
cat("Manual Edge Density:", ecount(g1) / (vcount(g1) * (vcount(g1) - 1)), "\n")

# Reciprocity
cat("Reciprocity:", reciprocity(g1), "\n")

# Closeness centrality
cat("\n===== Closeness Centrality =====\n")
print(closeness(g1, mode = 'all', weights = NA))

# Betweenness centrality
cat("\n===== Betweenness Centrality =====\n")
print(betweenness(g1, directed = T, weights = NA))

# Edge betweenness
cat("\n===== Edge Betweenness =====\n")
print(edge_betweenness(g1, directed = T, weights = NA))

# -----------------------------------------------------------
# Part 4: Read the Network Data File
# -----------------------------------------------------------
data <- read.csv('https://raw.githubusercontent.com/bkrai/R-files-from-YouTube/main/networkdata.csv',
                 header = T)

cat("\n===== Dataset Preview =====\n")
print(head(data))
cat("Total rows:", nrow(data), "\n")

y <- data.frame(data$first, data$second)

# -----------------------------------------------------------
# Part 5: Create the Network from Data
# -----------------------------------------------------------
net <- graph.data.frame(y, directed = T)
V(net)$label <- V(net)$name
V(net)$degree <- degree(net)

# -----------------------------------------------------------
# Part 6: Histogram of Node Degree
# -----------------------------------------------------------
hist(V(net)$degree,
     col = "steelblue",
     main = "Histogram of Node Degree",
     xlab = "Degree",
     ylab = "Frequency")

# -----------------------------------------------------------
# Part 7: Network Diagram – Basic
# -----------------------------------------------------------
plot(net,
     main = "Network Diagram – Basic")

# -----------------------------------------------------------
# Part 8: Network Diagram – Highlighting Degrees & Layouts
# -----------------------------------------------------------
plot(net,
     vertex.color = rainbow(52),
     vertex.size  = V(net)$degree * 0.4,
     edge.arrow.size = 0.1,
     layout = layout.fruchterman.reingold,
     main   = "Network – Nodes Sized by Degree (Fruchterman-Reingold)")

# -----------------------------------------------------------
# Part 9: Hub and Authority Scores
# -----------------------------------------------------------
hs <- hub_score(net)$vector
as <- authority.score(net)$vector

par(mfrow = c(1, 2))

set.seed(123)

plot(net,
     vertex.size  = hs * 30,
     main = 'Hubs',
     vertex.color = rainbow(52),
     edge.arrow.size = 0.1,
     layout = layout.kamada.kawai)

plot(net,
     vertex.size  = as * 30,
     main = 'Authorities',
     vertex.color = rainbow(52),
     edge.arrow.size = 0.1,
     layout = layout.kamada.kawai)

par(mfrow = c(1, 1))

# -----------------------------------------------------------
# Part 10: Community Detection
# -----------------------------------------------------------
net <- graph.data.frame(y, directed = F)
cnet <- cluster_edge_betweenness(net)

plot(cnet, net,
     main = "Community Detection (Edge Betweenness)")

cat("\n===== Community Detection Results =====\n")
cat("Number of communities:", length(cnet), "\n")
cat("Modularity:", round(modularity(cnet), 4), "\n")
cat("\nCommunity memberships:\n")
print(membership(cnet))

# -----------------------------------------------------------
# Summary / Interpretation
# -----------------------------------------------------------
cat("\n=============================================================\n")
cat("       INTERPRETATION OF RESULTS                            \n")
cat("=============================================================\n\n")

cat("1. The small directed graph (Amy, Ram, Li, Kate) demonstrates\n")
cat("   basic network properties — degree, closeness, betweenness,\n")
cat("   reciprocity, and edge density.\n\n")

cat("2. The larger network from networkdata.csv contains 52 nodes\n")
cat("   with directed relationships. The degree histogram shows\n")
cat("   how connections are distributed across nodes.\n\n")

cat("3. Hub scores identify nodes that point to many important\n")
cat("   authorities. Authority scores identify nodes that are\n")
cat("   pointed to by many important hubs.\n\n")

cat("4. Community detection using edge betweenness clustering\n")
cat("   reveals natural groupings within the undirected version\n")
cat("   of the network — nodes within the same community are\n")
cat("   more densely connected to each other than to outsiders.\n\n")

cat("=============================================================\n")
cat("              Analysis Complete.                             \n")
cat("=============================================================\n")
