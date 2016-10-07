
# code for adding a "tree score" to each Manhattan street segment.

library(data.table)
library(dplyr)
library(mgcv)

trees = fread('../data/TreesCount2015Trees.csv')
blocks = fread('../output/blocks_Manhattan.csv')
node_list = fread('../output/node_list.csv')
manhattan_outline = fread('../data/Boundary.csv')
source('../lib/assign_to_segment.R')

trees = select(trees, lon=longitude, lat=latitude) # remove useless columns
trees = as.matrix(trees)
manhattan_outline = as.matrix(manhattan_outline)
trees = trees[in.out(manhattan_outline, trees),] # select manhattan trees only
node_list = filter(node_list, segment_id %in% blocks$PHYSICALID) # manhattan nodes only


ptm <- proc.time()
a = apply(trees[1:100,], 1, FUN=assign_to_segment, node_list)
proc.time() - ptm  
  
# add lon and lat of segment end points as columns:
trees = mutate(trees, 
                start_lon = sapply(the_geom, get_end_nodes,'start_lon'),
                start_lat = sapply(the_geom, get_end_nodes,'start_lat'),
                end_lon = sapply(the_geom, get_end_nodes,'end_lon'),
                end_lat = sapply(the_geom, get_end_nodes,'end_lat') )



# add unique IDs to segment end points based on their coords rounded to the 5th decimal:
blocks = blocks %>% 
          mutate(start_ID = paste(as.character(round(start_lon, 5)), as.character(round(start_lat, 5)))) %>% 
          mutate(end_ID = paste(as.character(round(end_lon, 5)), as.character(round(end_lat, 5))))


write.csv(blocks, file = '../output/blocks_Manhattan.csv', row.names = FALSE)


# # for checking that it works:
# library(leaflet)
# block = filter(blocks, PHYSICALID==3188) # enter any block ID here
# 
# m <- leaflet() %>%
#       addTiles() %>%
#       addMarkers(lng=block$start_lon , lat=block$start_lat) %>%
#       addMarkers(lng=block$end_lon , lat=block$end_lat)
# m



