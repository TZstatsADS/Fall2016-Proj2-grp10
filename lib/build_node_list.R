
# Create list (dataframe) of all nodes in map, with their lon-lat coordinates and their associated segment

library(data.table)

blocks = fread('../data/street_blocks.csv')
blocks = blocks[!duplicated(blocks$PHYSICALID),]

nodes_from_seg = function(seg){ # extract numeric coordinates of all nodes in a 'the_geom' segment
  seg = gsub("LINESTRING \\(", "", seg)
  seg = gsub("\\)", "", seg)
  nodes = strsplit(seg, ', ')
  
  node_coords = data.frame(lon=NA, lat=NA)
  for(i in 1:length(nodes[[1]])){
    node_coords[i,] = as.double(strsplit(nodes[[1]][i], ' ')[[1]])
  }
  return(node_coords)
}

all_nodes = data.frame()
for(i in 1:nrow(blocks)){
  segment_id = blocks$PHYSICALID[i]
  segment_nodes = nodes_from_seg(blocks$the_geom[i])
  n_nodes = nrow(segment_nodes)
  node_ids = paste(segment_id,'_', 1:n_nodes, sep='') # create unique node ids in the form segmentID_node#
  
  segment_nodes = data.frame(node_id=node_ids, segment_id=rep(segment_id,n_nodes), segment_nodes)
  
  all_nodes = rbind(all_nodes, segment_nodes)
}

write.csv(all_nodes, file = '../output/node_list.csv', row.names = FALSE)

