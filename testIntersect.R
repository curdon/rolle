
#intersecting data

library(sp)

#all data requires a unique identifier 
rolle_build_osm$iid<-1:nrow(rolle_build_osm)
rolle_gwr$iid<-1:nrow(rolle_gwr)
rolle_build$iid<-1:nrow(rolle_build)

####osm search----

#spatial intersection of osm with gwr
#the resulting df contains a list element per osm building
#for each building the intersecting gwr$iid's, if any, are known 
ov<-sp::over(rolle_build_osm[,"iid"],rolle_gwr[,"iid"], returnList = TRUE)

#iterating through all lists and retrieve
#osm without gwr buildings: osm_empty
#osm that have one gwr building: osm_ok
#osm that have multiple gwr buildings: osm_mult
osm_empty<-numeric()
osm_ok<-numeric()
osm_mult<-numeric()

#gwr that do not intersect with osm: gwr_empty
#gwr that intersect with one osm: gwr_ok
#gwr that intersect with multiple osm (can happen if osm polygons overlap): gwr_mult
gwr_empty<-numeric()
gwr_ok<-numeric()
gwr_mult<-numeric()

#the two types of intersection - one gwr on one osm & multiple gwr on one osm - are stored in one df with respective iid's
gwr_on_osm_ok<-data.frame(iidOsm=NA,iidGwr=NA)
gwr_on_osm_multi<-data.frame(iidOsm=NA,iidGwr=NA)

#start iteration
for(i in 1:length(ov)){
  ov.i<-ov[[i]]
  
  #no gwr in osm
  if(nrow(ov.i)==0){
    
    osm_empty<-c(osm_empty,i)
    
    #exactly one gwr in osm
  }else if(nrow(ov.i)==1){
    
    osm_ok<-c(osm_ok,i)
    gwr_ok<-c(gwr_ok,ov.i$iid)
    
    gwr_on_osm_ok<-rbind(gwr_on_osm_ok,data.frame(iidOsm=i,iidGwr=ov.i$iid))
    
    #several gwr in one osm
  }else if(nrow(ov.i)>1){
    
    osm_mult<-c(osm_mult,i)
    gwr_mult<-c(gwr_mult,ov.i$iid)
    
    gwr_on_osm_multi<-rbind(gwr_on_osm_multi,data.frame(iidOsm=rep(i,nrow(ov.i)),iidGwr=ov.i$iid))
    
  }
}

gwr_on_osm_ok<-gwr_on_osm_ok[-1,]
gwr_on_osm_multi<-gwr_on_osm_multi[-1,]

#there are now still duplicates in terms of gwr points that intersect with multiple buildings
#these duplicates are not resolved at the moment, 
#however, they cause that the sum of empty+OK+multiple equals to more than the number of gwr buildings

un<-unique(c(gwr_ok,gwr_mult))
gwr_empty<-rolle_gwr@data[!rolle_gwr$iid%in%un,"iid"]


####tlm search----

#those gwr that did not intersect with osm are now intersected with tlm geometries
rolle_gwr_rest<-rolle_gwr[rolle_gwr$iid%in%gwr_empty,]
ov<-sp::over(rolle_build[,"iid"],rolle_gwr_rest[,"iid"], returnList = TRUE)

#same as above
tlm_empty<-numeric()
tlm_ok<-numeric()
tlm_mult<-numeric()

gwr_tlm_empty<-numeric()
gwr_tlm_ok<-numeric()
gwr_tlm_mult<-numeric()

gwr_on_tlm_ok<-data.frame(iidTlm=NA,iidGwr=NA)
gwr_on_tlm_multi<-data.frame(iidTlm=NA,iidGwr=NA)

for(i in 1:length(ov)){
  ov.i<-ov[[i]]
  
  if(nrow(ov.i)==0){
    
    tlm_empty<-c(tlm_empty,i)
    
  }else if(nrow(ov.i)==1){
    
    tlm_ok<-c(tlm_ok,i)
    gwr_tlm_ok<-c(gwr_tlm_ok,ov.i$iid)
    gwr_on_tlm_ok<-rbind(gwr_on_tlm_ok,data.frame(iidTlm=i,iidGwr=ov.i$iid))
    
  }else if(nrow(ov.i)>1){
    
    tlm_mult<-c(tlm_mult,i)
    gwr_tlm__mult<-c(gwr_mult,ov.i$iid)
    
    gwr_on_tlm_multi<-rbind(gwr_on_tlm_multi,data.frame(iidTlm=rep(i,nrow(ov.i)),iidGwr=ov.i$iid))
    
  }
}

gwr_on_tlm_ok<-gwr_on_tlm_ok[-1,]
gwr_on_tlm_multi<-gwr_on_tlm_multi[-1,]

un<-unique(c(gwr_tlm_ok,gwr_tlm_mult))
gwr_tlm_empty<-rolle_gwr_rest@data[!rolle_gwr_rest$iid%in%un,"iid"]


####putting it together----

#randomly removing osm or tlm that recieved multiple gwrs...
#should be improved in future
gwr_on_tlm_ok_clean<-gwr_on_tlm_ok[!duplicated(gwr_on_tlm_ok$iidGwr), ]
gwr_on_osm_ok_clean<-gwr_on_osm_ok[!duplicated(gwr_on_osm_ok$iidGwr), ]
gwr_on_tlm_mult_clean<-gwr_on_tlm_mult[!duplicated(gwr_on_tlm_mult$iidGwr), ]
gwr_on_osm_mult_clean<-gwr_on_osm_mult[!duplicated(gwr_on_osm_mult$iidGwr), ]

#all ok tlm and osm polygons
#first osm...
rolle_osm_gwr<-rolle_build_osm[rolle_build_osm$iid%in%gwr_on_osm_ok_clean$iidOsm,]
rolle_osm_gwr@data<-merge(rolle_osm_gwr@data,gwr_on_osm_ok_clean,by.x="iid",by.y="iidOsm")
rolle_osm_gwr@data<-merge(rolle_osm_gwr@data,rolle_gwr@data,by.x="iidGwr",by.y="iid")

#...then tlm
rolle_tlm_gwr<-rolle_build[rolle_build$iid%in%gwr_on_tlm_ok_clean$iidTlm,]
rolle_tlm_gwr@data<-merge(rolle_tlm_gwr@data,gwr_on_tlm_ok_clean,by.x="iid",by.y="iidTlm")
rolle_tlm_gwr@data<-merge(rolle_tlm_gwr@data,rolle_gwr@data,by.x="iidGwr",by.y="iid")

#and those gwr that did not match with neigher tlm or gwr
rolle_gwr_empty<-rolle_gwr[rolle_gwr$iid%in%gwr_tlm_empty,]


