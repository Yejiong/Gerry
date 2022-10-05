
library(sf)          # classes and functions for vector data
library(spData)        # load geographic data
library(dplyr)
library(tmap)    # for static and interactive maps
library(ggplot2) 
library(tools)
library(readr)

process<-function(state){
  #state="tx"
  ######### read in congressional district map 2010
  file=paste("congressional2010/",state,"_cong_2012_to_2021/",state,"_cong_2012_to_2021.shp",sep="")
  cong = read_sf(file, quiet = TRUE)
  cong= st_transform(cong, "EPSG:3857")
   
  
  # read in block boundary
  file=paste("block2010/",state,"_2010_b_bound/",state,"_2010_b_bound.shp",sep="")
  block = read_sf(file, quiet = TRUE)
  block= st_transform(block, "EPSG:3857")
   
  
  ######### district affliation
  # centroids by blocks, output a vector
  blockcentroid=st_centroid(block$geometry)
  # district belonging matrix, output a sparse matrix
  belong_mat=matrix(NA,nrow=dim(block)[1],ncol=dim(cong)[1])
  for(d in 1:dim(cong)[1]){
    belong_mat[,d]=st_within(blockcentroid, cong[d,], sparse = FALSE)
  }
  # add a new colum to the block dataset indicating dist
  dist=rep(NA,dim(block)[1])
  
  library(foreach)
  foreach (i=1:3) %do% {
    sqrt(i)
  }
  
  
  for(i in 1:dim(block)[1]){
    if(sum(belong_mat[i,]==TRUE)>0){dist[i]=which(belong_mat[i,]==TRUE)}
    else{
      dist[i]=NA
    }
  }
  block$dist=dist
  
  ####### read in voters at block level
  file=paste("voter2021/",toupper(state),"_L2_Comm_2010BlockAgg/",toupper(state),"_l2_comm_block_agg_20210419.csv",sep="")
  voter=read_csv(file,col_types = cols(geoid = col_character())) 
  #names(voter)
  
  # link block with voter using geoid, voter live only in half of the region
  block = block%>%left_join(voter,by=c("GEOID"="geoid"))
  
  # remove nonresident blocks
  block = block%>%filter(!is.na(total_reg))
  
  
  ###### calculate voting power: each district has 1 unit of power, divide it among the winners
  # each block's expected voting weight = winner's weight * prop winner + loser's weight * prop loser
  distpower=st_drop_geometry(block)%>%
    select(dist,party_dem,party_rep,total_reg)%>%
    group_by(dist)%>%
    summarise(dem=sum(party_dem), rep=sum(party_rep), tot=sum(total_reg))%>%
    ungroup()%>%mutate(rep.wt=ifelse(rep>dem, 1/rep, 0), 
                       dem.wt=ifelse(rep<dem, 1/dem, 0))
  
  
  
  blockpower=block%>%left_join(distpower, by="dist")%>%
    mutate(block.power=dem.wt*(party_dem/(party_rep+party_dem))+rep.wt*(party_rep/(party_rep+party_dem)))%>%
    mutate(aa.ratio=(eth1_aa)/total_reg, hisp.ratio=(eth1_hisp)/total_reg )%>%
    mutate(race.group=ifelse(aa.ratio>=0.5,"AA",ifelse(hisp.ratio>=0.5,"Hispanic","Others")))
  
  
  
  return(list(blockpower,p1,p2))
}
tx.data=process("tx")
tx.data[[2]]
tx.data[[3]]

p1=st_drop_geometry(blockpower)%>%
  select(race.group,aa.ratio,hisp.ratio,block.power)%>%
  ggplot()+
  geom_density(aes(block.power,group=race.group,col=race.group))+
  xlim(c(0,0.00001))


#### visualize power
p2=tm_shape(blockpower) +
  tm_polygons("race.group")+
  tm_dots("block.power",size=0.01,alpha=0.1)+
  tm_shape(cong)+
  tm_borders(col='red')


 
 