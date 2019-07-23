library(sf)

## https://geodata.lib.berkeley.edu/catalog/nyu_2451_37053
## https://library.carleton.ca/find/gis/geospatial-data/shapefiles-canada-united-states-and-world

# tam <- read_sf("statemaps/tamps_entidad.shp")
# 
# plot(tam)
# 

lower_lat <- 24.5


### This worked fine but st_crop is much less code

# clipstate <- function (stateshp, lower_lat = 24.5) {
#   bstateclip <- st_bbox(stateshp)  # Get bounding box of State
#   
#   nw <- st_point(c(bstateclip$xmax,bstateclip$ymin))
#   ne <- st_point(c(bstateclip$xmin,bstateclip$ymin))
#   se <- st_point(c(bstateclip$xmin,lower_lat))
#   sw <- st_point(c(bstateclip$xmax,lower_lat))
#   
#   recto <- list(rbind(nw,ne,se,sw,nw))
#   
#   cutterp <- st_polygon(recto)
#   cutterpf <- st_sfc(cutterp, crs=4326)
#   st_crs(cutterpf) <- st_crs(stateshp)
#   upclipped <- st_difference(stateshp,cutterpf)
#   
#   return(upclipped)
# }


### Simpler method that does not require the bounding box of the input shape:

cropstate <- function (prov, lower_lat = 24.5) {
  ## Here  upper_lat is the upper latitude of the included section. 
  prov <- st_transform(prov,crs=4326)
  maybesmallerpoly <- st_crop(prov, c(xmin=-124.8, xmax=-66.5, ymin=lower_lat, ymax=32.72))
  return(maybesmallerpoly)
}


## We were using clipstate, swapping in cropstate b passing in the function
##                          amazed this worked
process_state <- function (inpath, outname, method=cropstate) {
  shaper <- read_sf(inpath)
  clipped <- method(shaper)
  write_sf(clipped,outname)
  return(clipped)
}


checkshape <- process_state("statemaps/chih_entidad.shp","Chihuahua.shp") 

plot(st_geometry(checkshape))

checkshape <- process_state("statemaps/coah_entidad.shp","Coahuila.shp") 
checkshape <- process_state("statemaps/nl_entidad.shp","NuevoLeon.shp") 
checkshape <- process_state("statemaps/bcs_entidad.shp","BahaSouth.shp") 
checkshape <- process_state("statemaps/zac_entidad.shp","Zacatecas.shp") 
#checkshape <- #process_state("statemaps/slp_entidad.shp","SLPotosi.shp") 
checkshape <- process_state("statemaps/dgo_entidad.shp","Durango.shp") 
checkshape <- process_state("statemaps/sin_entidad.shp","Sinaloa.shp") 
checkshape <- process_state("statemaps/bc_entidad.shp","BahaCalifornia.shp")
checkshape <- process_state("statemaps/son_entidad.shp","Sonora.shp")
checkshape <- process_state("statemaps/tamps_entidad.shp","Tamaulipas.shp")



#### Unification
readclip <- function(inpath, method=cropstate) {
  shaper <- read_sf(inpath)
  clipped <- method(shaper)
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

write_sf(mex10,"~/Downloads/mexiconorte1.shp")


### This worked great:
mex1 <- st_union(BahaCalifornia, BahaSouth, by_feature = FALSE)
plot(mex1)
write_sf(mex1, "mxbaja.shp")

