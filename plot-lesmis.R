library(jsonlite) # to parse JSON data
library(igraph) # to make a symmetric matrix

# read les mis json data
json_data <- fromJSON("https://bost.ocks.org/mike/miserables/miserables.json")

characters <- json_data[[1]]$name
edges <- json_data[[2]]

edges_w_names <- t(apply(edges, MARGIN=1, function(x) {
  src_idx <- as.numeric(x["source"]+1)
  dest_idx <- as.numeric(x["target"]+1)
  c(characters[src_idx], characters[dest_idx], x["value"])
}))
edges_w_names <- as.data.frame(edges_w_names, stringsAsFactors=F)

colnames(edges_w_names) <- c("source", "target", "value")
edges_w_names$source <- as.character(edges_w_names$source)
edges_w_names$target <- as.character(edges_w_names$target)
edges_w_names$value <- as.numeric(edges_w_names$value)

gdf <- graph.data.frame(edges_w_names, directed=F)
adj_mat <- get.adjacency(gdf, sparse=F, attr="value")

# cluster graph and re-order matrix rows/columns by communities
members <- igraph::cluster_infomap(gdf)$membership
adj_mat <- adj_mat[order(members), order(members)]
n_cluster <- length(unique(members))

# plot code below
png("lesmis.png", width = 7000, height = 7000)

par(bg = 'black', 
    mar = c(50, 50, 50, 50), 
    family = 'Roboto Light', 
    col = 'white', 
    col.main = 'white', 
    ps = 300)

plot(axes = F, 
     0, 
     type = 'n', 
     xlim = c(1, ncol(adj_mat)), 
     ylim = c(1, ncol(adj_mat)), 
     main = "Les Misérables Co-occurance Matrix"
)

cluster_cols <- categorical_pal(n_cluster)

lapply(1:nrow(adj_mat), function(i) {
    lapply(1:ncol(adj_mat), function(j) {
        # each entry in matrix is an edge so if communities differ pick the smallest
        cluster_no <- min(sort(members)[j], sort(members)[i])
        rect(
            xleft = i - .5, 
            xright = i + .5, 
            ytop = j + .5, 
            ybottom = j - .5, 
            pch = '+', 
            cex = .5, 
            col=ifelse(adj_mat[i,j]==0, 'black', cluster_cols[cluster_no])
        )})
})

axis(tick=T, side = 1, labels = colnames(adj_mat), at = 1:nrow(adj_mat),
     col = "white",
     col.ticks = "white", col.axis="white", las=2, cex.axis=0.2)
axis(tick=T, side = 2, labels = rownames(adj_mat), at = 1:ncol(adj_mat), col = "white", col.ticks = "white", col.axis="white", las=1, cex.axis=0.2)
abline(h=1:ncol(adj_mat), v=1:ncol(adj_mat), lty=3)

dev.off()
