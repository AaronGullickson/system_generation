library(DiagrammeR)

get_network <- function(hpg_data) {
  first_circuit <- subset(hpg_data, hpg=="A" & faction_type=="IS" & is.na(abandon_year))
  outer_circuit <- subset(hpg_data, hpg!="A" & faction_type=="IS" & founding_year<2700)
  
  x <- matrix(rep(first_circuit$x, nrow(first_circuit)), 
              nrow(first_circuit), nrow(first_circuit))
  y <- matrix(rep(first_circuit$y, nrow(first_circuit)), 
              nrow(first_circuit), nrow(first_circuit))
  distance_matrix <- sqrt((x-t(x))^2+(y-t(y))^2)
  edges_matrix <- distance_matrix<=50
  diag(edges_matrix) <- FALSE
  edges <- NULL
  #TODO: how to do this not in a for-loop
  for(i in 1:nrow(edges_matrix)) {
    for(j in i:ncol(edges_matrix)) {
      if(edges_matrix[i,j]) {
        edges <- rbind(edges, c(i,j))
      }
    }
  }
  edges <- data.frame(from=first_circuit$id[edges[,1]], 
                      to=first_circuit$id[edges[,2]])
  edges$from <- as.character(edges$from)
  edges$to <- as.character(edges$to)
  
  #create graph
  h_graph <-create_graph(directed = FALSE)
  i_graph <- h_graph %>% 
    add_nodes_from_table(
      table = first_circuit,
      label_col = id)
  i_graph2 <- i_graph %>%
    add_edges_from_table(
      table=edges,
      from_col = from,
      to_col = to,
      from_to_map = label)
  
  first_circuit$connect_terra <- FALSE
  first_circuit$connect_terra[get_all_connected_nodes(i_graph2, 1)] <- TRUE
  first_circuit$connect_terra[1] <- TRUE
  
  return(list(first=first_circuit, 
              outer=outer_circuit,
              network=i_graph2))
}

#find closest points within two unconnected networks
find_closest_points <- function(main, isolate) {
  x_main <- matrix(rep(main$x, nrow(isolate)), 
                   nrow(main), nrow(isolate))
  y_main <- matrix(rep(main$y, nrow(isolate)), 
                   nrow(main), nrow(isolate))
  x_iso <- t(matrix(rep(isolate$x, nrow(main)), 
                    nrow(isolate), nrow(main)))
  y_iso <- t(matrix(rep(isolate$y, nrow(main)), 
                    nrow(isolate), nrow(main)))
  distances <- sqrt((x_main-x_iso)^2+(y_main-y_iso)^2)
  idx <- which(distances==min(distances), arr.ind=TRUE)
  terra_closest <- main[idx[1],]
  iso_closest <- isolate[idx[2],]
  return(list(terra=terra_closest,
              iso=iso_closest))
}

#find all outer circuit planets within 50LY of both points
find_all_overlaps <- function(closest, distance=50, outer=hpg_network$outer) {
  distance_main <- sqrt((outer$x-closest$terra$x)^2+(outer$y-closest$terra$y)^2)
  distance_iso <- sqrt((outer$x-closest$iso$x)^2+(outer$y-closest$iso$y)^2)
  candidates <- outer[distance_main<=distance & distance_iso<=distance,]
  return(candidates)
}

#find all outer circuit planets that get you closer to the main network
find_closer_planets <- function(closest, distance=50, outer=hpg_network$outer) {
  outer$distance_main <- sqrt((outer$x-closest$terra$x)^2+(outer$y-closest$terra$y)^2)
  outer$distance_iso <- sqrt((outer$x-closest$iso$x)^2+(outer$y-closest$iso$y)^2)
  distance_current <- sqrt((closest$iso$x-closest$terra$x)^2+(closest$iso$y-closest$terra$y)^2)
  candidates <- outer[outer$distance_iso<=distance & outer$distance_main<distance_current,]
  return(candidates)
}
