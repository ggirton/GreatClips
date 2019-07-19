library(sf)

## https://geodata.lib.berkeley.edu/catalog/nyu_2451_37053
## https://library.carleton.ca/find/gis/geospatial-data/shapefiles-canada-united-states-and-world

tam <- read_sf("statemaps/tamps_entidad.shp")

plot(tam)

bp = st_sfc(st_point(c(93,24.5)), st_point(c(124,24.5))) # create 2 points
bf = st_buffer(bp, dist = 0.5) # convert points to circles


bb = st_bbox(bf)

btamclip <- st_bbox(tam)  # Get bounding box of Tam

btambox = st_as_sfc(btamclip)


plot(cutterp)

bp <- btamclip

btambox
lower_lat <- 24.5

nw <- st_point(c(bp$xmax,bp$ymin))
ne <- st_point(c(bp$xmin,bp$ymin))
#se <- st_point(c(bp$xmin,bp$ymax))
#sw <- st_point(c(bp$xmax,bp$ymax))
se <- st_point(c(bp$xmin,lower_lat))
sw <- st_point(c(bp$xmax,lower_lat))

recto <- list(rbind(nw,ne,se,sw,nw))

cutterp <- st_polygon(recto)
cutterpf <- st_sfc(cutterp, crs=4326)
st_crs(cutterpf) <- st_crs(tam)
uptam <- st_difference(tam,cutterpf)

plot(uptam)

write_sf(uptam,"Tamaulipas.shp")

write_sf(uptam,"Tamaulipastop.kml")


####
#Sonora
son <- read_sf("statemaps/son_entidad.shp")

plot(son)

clipstate <- function (stateshp, lower_lat = 24.5) {
  bstateclip <- st_bbox(stateshp)  # Get bounding box of State
  
  nw <- st_point(c(bstateclip$xmax,bstateclip$ymin))
  ne <- st_point(c(bstateclip$xmin,bstateclip$ymin))
  se <- st_point(c(bstateclip$xmin,lower_lat))
  sw <- st_point(c(bstateclip$xmax,lower_lat))
  
  recto <- list(rbind(nw,ne,se,sw,nw))
  
  cutterp <- st_polygon(recto)
  cutterpf <- st_sfc(cutterp, crs=4326)
  st_crs(cutterpf) <- st_crs(stateshp)
  upclipped <- st_difference(stateshp,cutterpf)
  
  return(upclipped)
}

upclipped <- clipstate(son)


plot(upclipped)

write_sf(upclipped,"Sonora.shp")

write_sf(upclipped,"Sonoratop.kml")

procstat <- function (inpath, outname) {
  shaper <- read_sf(inpath)
  clipped <- clipstate(shaper)
  write_sf(clipped,outname)
}



#bc_entidad.shp
#chih_entidad.shp
#coah_entidad.shp
#nl_entidad.shp
#son_entidad.shp
#tamps_entidad.shp

procstat("statemaps/chih_entidad.shp","Chihuahua.shp") 
procstat("statemaps/coah_entidad.shp","Coahuila.shp") 
procstat("statemaps/nl_entidad.shp","NuevoLeon.shp") 

procstat("statemaps/bcs_entidad.shp","BahaSouth.shp") 


procstat("statemaps/zac_entidad.shp","Zacatecas.shp") 

procstat("statemaps/slp_entidad.shp","SLPotosi.shp") 

procstat("statemaps/dgo_entidad.shp","Durango.shp") 

procstat("statemaps/sin_entidad.shp","Sinaloa.shp") 

procstat("statemaps/bc_entidad.shp","BahaCalifornia.shp")
procstat("statemaps/son_entidad.shp","Sonora.shp")
procstat("statemaps/tamps_entidad.shp","Tamaulipas.shp")



#### Unification
readclip <- function(inpath) {
  shaper <- read_sf(inpath)
  clipped <- clipstate(shaper)
  return(clipped)
}

Chihuahua    = readclip("statemaps/chih_entidad.shp")  
Coahuila    = readclip("statemaps/coah_entidad.shp")  
NuevoLeon    = readclip("statemaps/nl_entidad.shp")    
BahaSouth    = readclip("statemaps/bcs_entidad.shp")   
Zacatecas    = readclip("statemaps/zac_entidad.shp")   
Durango    = readclip("statemaps/dgo_entidad.shp")   
Sinaloa    = readclip("statemaps/sin_entidad.shp")   
BahaCalifornia    =readclip("statemaps/bc_entidad.shp")    
Sonora    =readclip("statemaps/son_entidad.shp")   
Tamaulipas    =readclip("statemaps/tamps_entidad.shp") 

mex10 <- rbind(BahaCalifornia,BahaSouth,Chihuahua,
              Coahuila,Durango,NuevoLeon, Sinaloa, Sonora,
              Tamaulipas,Zacatecas)

write_sf(mex10,"mexiconorte.shp")

mex1 <- st_union(BahaCalifornia, BahaSouth, by_feature = FALSE)

plot(mex1)
write_sf(mex1, "mxbaja.shp")

