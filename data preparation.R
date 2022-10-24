
library(sf)          # classes and functions for vector data
library(spData)        # load geographic data
library(dplyr)
library(tmap)    # for static and interactive maps
library(ggplot2) 
library(ggsci)
library(tools)
library(readr)

library(foreach)
library(doParallel)

registerDoParallel(detectCores())
getDoParWorkers()
process<-function(state){
  
   
  #state="ca"
  ######### read in congressional district map 2010

  folder=grepl(state,list.files("/home/zhoux104/R/Gerry/congressional2010"))
  path=paste("/home/zhoux104/R/Gerry/congressional2010/",list.files("/home/zhoux104/R/Gerry/congressional2010")[folder],sep='')
  file=list.files(path)[grepl("shp",list.files(path))]
  cong = read_sf(paste(path, file,sep="/"), quiet = TRUE)
  cong= st_transform(cong, "EPSG:3857")
  
  
  # read in block boundary
  folder=grepl(state,list.files("/home/zhoux104/R/Gerry/block2010"))
  path=paste("/home/zhoux104/R/Gerry/block2010/",list.files("/home/zhoux104/R/Gerry/block2010")[folder],sep='')
  file=list.files(path)[grepl("shp",list.files(path))]
  block = read_sf(paste(path, file,sep="/"), quiet = TRUE)
  block= st_transform(block, "EPSG:3857")
  
  
  ######### district affliation
  # centroids by blocks, output a vector
  blockcentroid=st_centroid(block$geometry)
  # district belonging matrix, output a sparse matrix
  belong_mat=matrix(NA,nrow=dim(block)[1],ncol=dim(cong)[1])
  
  belong_mat = foreach(d = 1:dim(cong)[1], .combine = 'cbind',.packages = "sf")%dopar%{
    st_within(blockcentroid, cong[d,], sparse = FALSE)
  }
  
  
  # add a new colum to the block dataset indicating dist
  dist = foreach(i = 1:dim(block)[1], .combine =c)%dopar%{
    ifelse(sum(belong_mat[i,]==TRUE)>0,which(belong_mat[i,]==TRUE), NA )
  }
  block$dist=dist
  
  ####### read in voters at block level
  folder=grepl(state,list.files("/home/zhoux104/R/Gerry/voter2021"),ignore.case =TRUE)
  path=paste("/home/zhoux104/R/Gerry/voter2021/",list.files("/home/zhoux104/R/Gerry/voter2021")[folder],sep='')
  file=list.files(path)[grepl("csv",list.files(path))]
  voter=read_csv(paste(path, file,sep="/"),col_types = cols(geoid = col_character()))
  #names(voter)
  
  
  # link block with voter using geoid, voter live only in half of the region
  block = block%>%left_join(voter,by=c("GEOID"="geoid"))
  #View(block)
  # remove nonresident blocks
  block = block%>%filter(!is.na(total_reg))
  
  
  ###### calculate prob of voter i is satisfied = P(R=vi) this is constant within district
  # distpower=st_drop_geometry(block)%>%
  #    select(dist,party_dem,party_rep,total_reg)%>%
  #    group_by(dist)%>%
  #    summarise(dem=sum(party_dem), rep=sum(party_rep), tot=sum(total_reg))%>%
  #    ungroup()%>%mutate(Dwin=(dem+(tot-dem-rep)/2)/(tot))
  # 
  # voter.power=block%>%left_join(distpower, by="dist")%>%
  #   mutate(black.power=Dwin*(0.87+(1-0.87-0.07)/2)+(1-Dwin)*(0.07+(1-0.87-0.07)/2),
  #          hisp.power=Dwin*(0.63+(1-0.63-0.27)/2)+(1-Dwin)*(0.27+(1-0.63-0.27)/2),
  #          white.power=Dwin*(0.39+(1-0.54-0.39)/2)+(1-Dwin)*(0.54+(1-0.54-0.39)/2),
  #          asian.power=Dwin*(0.72+(1-0.72-0.17)/2)+(1-Dwin)*(0.17+(1-0.72-0.17)/2))
  # 

  
  # distpower=st_drop_geometry(block)%>%
  #   select(dist,party_dem,party_rep,total_reg)%>%
  #   group_by(dist)%>%
  #   summarise(dem=sum(party_dem), rep=sum(party_rep), tot=sum(total_reg))%>%
  #   ungroup()%>%mutate(rep.wt=ifelse(rep>dem, rep/(rep+dem), 0),
  #                      dem.wt=ifelse(rep<dem, dem/(rep+dem), 0))
  
  # calculate expected seat share
  distpower=st_drop_geometry(block)%>%
    select(dist,party_dem,party_rep,total_reg)%>%
    group_by(dist)%>%
    summarise(dem=sum(party_dem), rep=sum(party_rep), tot=sum(total_reg))%>%
    ungroup()%>%mutate(rep.wt=ifelse(rep>dem, 1/(rep+(tot-dem-rep)/2), 0),
                       dem.wt=ifelse(rep<dem, 1/(dem+(tot-dem-rep)/2), 0))
  
   
  voter.power=block%>%left_join(distpower, by="dist")%>%
    mutate(black.power=dem.wt*(0.87+(1-0.87-0.07)/2)+rep.wt*(0.07+(1-0.87-0.07)/2),
           hisp.power=dem.wt*(0.63+(1-0.63-0.27)/2)+rep.wt*(0.27+(1-0.63-0.27)/2),
           white.power=dem.wt*(0.39+(1-0.54-0.39)/2)+rep.wt*(0.54+(1-0.54-0.39)/2),
           asian.power=dem.wt*(0.72+(1-0.72-0.17)/2)+rep.wt*(0.17+(1-0.72-0.17)/2))%>%
    mutate(black.power=black.power*tot/100,
           hisp.power=hisp.power*tot/100,
           white.power=white.power*tot/100,
           asian.power=asian.power*tot/100)
  
  #names(voter.power)
   
  # black.power=foreach(i = 1:dim(voter.power)[1], .combine =c)%dopar%{
  #   if(voter.power$eth1_aa[i]>0){rep(voter.power$black.power[i],voter.power$eth1_aa[i])}
  # }
  # hisp.power=foreach(i = 1:dim(voter.power)[1], .combine =c)%dopar%{
  #   if(voter.power$eth1_hisp[i]>0){rep(voter.power$hisp.power[i],voter.power$eth1_hisp[i])}
  # }
  # white.power=foreach(i = 1:dim(voter.power)[1], .combine =c)%dopar%{
  #   if(voter.power$eth1_eur[i]>0){rep(voter.power$white.power[i],voter.power$eth1_eur[i])}
  # }
  # asian.power=foreach(i = 1:dim(voter.power)[1], .combine =c)%dopar%{
  #   if(voter.power$eth1_esa[i]>0){rep(voter.power$asian.power[i],voter.power$eth1_esa[i])}
  # }
  # 
  # ind.power=data.frame(power=c(black.power,hisp.power,white.power,asian.power),
  #                      race=c(rep("Black",length(black.power)),rep("Hispanic",length(hisp.power)),rep("White",length(white.power)),rep("Asian",length(asian.power))),
  #                      state=state)
  # 
 
  
  return(voter.power)
}


### common states
state1=substr(list.files("/home/zhoux104/R/Gerry/congressional2010"),1,2)
state2=substr(list.files("/home/zhoux104/R/Gerry/block2010"),1,2)
state3=tolower(substr(list.files("/home/zhoux104/R/Gerry/voter2021"),1,2))
states=intersect(intersect(state1,state2),state3)
 
al.data=process("al")
ar.data=process("ar")
az.data=process("az")
ca.data=process("ca")
#co.data=process("co")
ct.data=process("ct")
fl.data=process("fl")
tx.data=process("tx")
ga.data=process("ga")
hi.data=process("hi")
ia.data=process("ia")
id.data=process("id")
il.data=process("il")
in.data=process("in")
ks.data=process("ks")
ky.data=process("ky")
la.data=process("la")
ma.data=process("ma")
md.data=process("md")
me.data=process("me")
mi.data=process("mi")
mn.data=process("mn")
mo.data=process("mo")
ms.data=process("ms")
nc.data=process("nc")
ne.data=process("ne")
nh.data=process("nh")
nj.data=process("nj")
nm.data=process("nm")
nv.data=process("nv")
ny.data=process("ny")
oh.data=process("oh")
ok.data=process("ok")
or.data=process("or")
pa.data=process("pa")
ri.data=process("ri")
sc.data=process("sc")
tn.data=process("tn")
ut.data=process("ut")
va.data=process("va")
wa.data=process("wa")
wi.data=process("wi")
wv.data=process("wv")

save(al.data, file = "data/al.data.RData")
save(ar.data, file = "data/ar.data.RData")
save(az.data, file = "data/az.data.RData")
save(ca.data, file = "data/ca.data.RData")
save(ct.data, file = "data/ct.data.RData")
save(tx.data, file = "data/tx.data.RData")
save(ga.data, file = "data/ga.data.RData")
save(hi.data, file = "data/hi.data.RData")
save(ia.data, file = "data/ia.data.RData")
save(id.data, file = "data/id.data.RData")
save(il.data, file = "data/il.data.RData")
save(in.data, file = "data/in.data.RData")
save(ks.data, file = "data/ks.data.RData")
save(ky.data, file = "data/ky.data.RData")
save(la.data, file = "data/la.data.RData")
save(ma.data, file = "data/ma.data.RData")
save(md.data, file = "data/md.data.RData")
save(me.data, file = "data/me.data.RData")
save(mi.data, file = "data/mi.data.RData")
save(mn.data, file = "data/mn.data.RData")
save(mo.data, file = "data/mo.data.RData")
save(ms.data, file = "data/ms.data.RData")
save(nc.data, file = "data/nc.data.RData")
save(ne.data, file = "data/ne.data.RData")
save(nh.data, file = "data/nh.data.RData")
save(nj.data, file = "data/nj.data.RData")
save(nm.data, file = "data/nm.data.RData")
save(nv.data, file = "data/nv.data.RData")
save(ny.data, file = "data/ny.data.RData")
save(oh.data, file = "data/oh.data.RData")
save(ok.data, file = "data/ok.data.RData")
save(or.data, file = "data/or.data.RData")
save(pa.data, file = "data/pa.data.RData")
save(ri.data, file = "data/ri.data.RData")
save(sc.data, file = "data/sc.data.RData")
save(tn.data, file = "data/tn.data.RData")
save(ut.data, file = "data/ut.data.RData")
save(va.data, file = "data/va.data.RData")
save(wa.data, file = "data/wa.data.RData")
save(wi.data, file = "data/wi.data.RData")
save(wv.data, file = "data/wv.data.RData")

names(al.data)
power.data=rbind(
  st_drop_geometry(al.data)[,c("STATE","hisp.power","black.power","asian.power","white.power","eth1_eur","eth1_aa","eth1_hisp","eth1_esa")],
  st_drop_geometry(ar.data)[,c("STATE","hisp.power","black.power","asian.power","white.power","eth1_eur","eth1_aa","eth1_hisp","eth1_esa")],
  st_drop_geometry(az.data)[,c("STATE","hisp.power","black.power","asian.power","white.power","eth1_eur","eth1_aa","eth1_hisp","eth1_esa")],
  st_drop_geometry(ca.data)[,c("STATE","hisp.power","black.power","asian.power","white.power","eth1_eur","eth1_aa","eth1_hisp","eth1_esa")],
  st_drop_geometry(ct.data)[,c("STATE","hisp.power","black.power","asian.power","white.power","eth1_eur","eth1_aa","eth1_hisp","eth1_esa")],
  st_drop_geometry(fl.data)[,c("STATE","hisp.power","black.power","asian.power","white.power","eth1_eur","eth1_aa","eth1_hisp","eth1_esa")],
  st_drop_geometry(tx.data)[,c("STATE","hisp.power","black.power","asian.power","white.power","eth1_eur","eth1_aa","eth1_hisp","eth1_esa")],
  st_drop_geometry(ga.data)[,c("STATE","hisp.power","black.power","asian.power","white.power","eth1_eur","eth1_aa","eth1_hisp","eth1_esa")],
  st_drop_geometry(hi.data)[,c("STATE","hisp.power","black.power","asian.power","white.power","eth1_eur","eth1_aa","eth1_hisp","eth1_esa")],
  st_drop_geometry(ia.data)[,c("STATE","hisp.power","black.power","asian.power","white.power","eth1_eur","eth1_aa","eth1_hisp","eth1_esa")],
  st_drop_geometry(id.data)[,c("STATE","hisp.power","black.power","asian.power","white.power","eth1_eur","eth1_aa","eth1_hisp","eth1_esa")],
  st_drop_geometry(il.data)[,c("STATE","hisp.power","black.power","asian.power","white.power","eth1_eur","eth1_aa","eth1_hisp","eth1_esa")],
  st_drop_geometry(in.data)[,c("STATE","hisp.power","black.power","asian.power","white.power","eth1_eur","eth1_aa","eth1_hisp","eth1_esa")],
  st_drop_geometry(ks.data)[,c("STATE","hisp.power","black.power","asian.power","white.power","eth1_eur","eth1_aa","eth1_hisp","eth1_esa")],
  st_drop_geometry(ky.data)[,c("STATE","hisp.power","black.power","asian.power","white.power","eth1_eur","eth1_aa","eth1_hisp","eth1_esa")],
  st_drop_geometry(la.data)[,c("STATE","hisp.power","black.power","asian.power","white.power","eth1_eur","eth1_aa","eth1_hisp","eth1_esa")],
  st_drop_geometry(ma.data)[,c("STATE","hisp.power","black.power","asian.power","white.power","eth1_eur","eth1_aa","eth1_hisp","eth1_esa")],
  st_drop_geometry(md.data)[,c("STATE","hisp.power","black.power","asian.power","white.power","eth1_eur","eth1_aa","eth1_hisp","eth1_esa")],
  st_drop_geometry(me.data)[,c("STATE","hisp.power","black.power","asian.power","white.power","eth1_eur","eth1_aa","eth1_hisp","eth1_esa")],
  st_drop_geometry(mi.data)[,c("STATE","hisp.power","black.power","asian.power","white.power","eth1_eur","eth1_aa","eth1_hisp","eth1_esa")],
  st_drop_geometry(mn.data)[,c("STATE","hisp.power","black.power","asian.power","white.power","eth1_eur","eth1_aa","eth1_hisp","eth1_esa")],
  st_drop_geometry(mo.data)[,c("STATE","hisp.power","black.power","asian.power","white.power","eth1_eur","eth1_aa","eth1_hisp","eth1_esa")],
  st_drop_geometry(ms.data)[,c("STATE","hisp.power","black.power","asian.power","white.power","eth1_eur","eth1_aa","eth1_hisp","eth1_esa")],
  st_drop_geometry(nc.data)[,c("STATE","hisp.power","black.power","asian.power","white.power","eth1_eur","eth1_aa","eth1_hisp","eth1_esa")],
  st_drop_geometry(ne.data)[,c("STATE","hisp.power","black.power","asian.power","white.power","eth1_eur","eth1_aa","eth1_hisp","eth1_esa")],
  st_drop_geometry(nh.data)[,c("STATE","hisp.power","black.power","asian.power","white.power","eth1_eur","eth1_aa","eth1_hisp","eth1_esa")],
  st_drop_geometry(nj.data)[,c("STATE","hisp.power","black.power","asian.power","white.power","eth1_eur","eth1_aa","eth1_hisp","eth1_esa")],
  st_drop_geometry(nm.data)[,c("STATE","hisp.power","black.power","asian.power","white.power","eth1_eur","eth1_aa","eth1_hisp","eth1_esa")],
  st_drop_geometry(nv.data)[,c("STATE","hisp.power","black.power","asian.power","white.power","eth1_eur","eth1_aa","eth1_hisp","eth1_esa")],
  st_drop_geometry(ny.data)[,c("STATE","hisp.power","black.power","asian.power","white.power","eth1_eur","eth1_aa","eth1_hisp","eth1_esa")],
  st_drop_geometry(oh.data)[,c("STATE","hisp.power","black.power","asian.power","white.power","eth1_eur","eth1_aa","eth1_hisp","eth1_esa")],
  st_drop_geometry(ok.data)[,c("STATE","hisp.power","black.power","asian.power","white.power","eth1_eur","eth1_aa","eth1_hisp","eth1_esa")],
  st_drop_geometry(or.data)[,c("STATE","hisp.power","black.power","asian.power","white.power","eth1_eur","eth1_aa","eth1_hisp","eth1_esa")],
  st_drop_geometry(pa.data)[,c("STATE","hisp.power","black.power","asian.power","white.power","eth1_eur","eth1_aa","eth1_hisp","eth1_esa")],
  st_drop_geometry(ri.data)[,c("STATE","hisp.power","black.power","asian.power","white.power","eth1_eur","eth1_aa","eth1_hisp","eth1_esa")],
  st_drop_geometry(sc.data)[,c("STATE","hisp.power","black.power","asian.power","white.power","eth1_eur","eth1_aa","eth1_hisp","eth1_esa")],
  st_drop_geometry(tn.data)[,c("STATE","hisp.power","black.power","asian.power","white.power","eth1_eur","eth1_aa","eth1_hisp","eth1_esa")],
  st_drop_geometry(ut.data)[,c("STATE","hisp.power","black.power","asian.power","white.power","eth1_eur","eth1_aa","eth1_hisp","eth1_esa")],
  st_drop_geometry(va.data)[,c("STATE","hisp.power","black.power","asian.power","white.power","eth1_eur","eth1_aa","eth1_hisp","eth1_esa")],
  st_drop_geometry(wa.data)[,c("STATE","hisp.power","black.power","asian.power","white.power","eth1_eur","eth1_aa","eth1_hisp","eth1_esa")],
  st_drop_geometry(wi.data)[,c("STATE","hisp.power","black.power","asian.power","white.power","eth1_eur","eth1_aa","eth1_hisp","eth1_esa")],
  st_drop_geometry(wv.data)[,c("STATE","hisp.power","black.power","asian.power","white.power","eth1_eur","eth1_aa","eth1_hisp","eth1_esa")]
)
 
 
save(power.data, file = "data/power.data.RData")
load("data/power.data.RData")

ind.data=ind.data%>%mutate(T=ifelse(state %in% c("ar","ca","hi","al","ar") ,1,0))

 

 



























 
 