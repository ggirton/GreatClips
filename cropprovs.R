library(sf)
library(dplyr)
library(ggplot2)

#Province only
# ocan <- read_sf("provincemap/lpr_000b16a_e.shp", stringsAsFactors = FALSE)

# Divisions
ocan <- read_sf("provincemap/lcsd000a17a_e.shp", stringsAsFactors = FALSE)


unique(ocan$PRNAME)

# length(ocan)
# nrow(ocan)

edgebo <- c("New Brunswick","Quebec","Ontario","Manitoba",
            "Saskatchewan","Alberta","British Columbia")
  
edge <- ocan[ocan$PRENAME %in% edgebo,]

edgeboduo <- c("New Brunswick / Nouveau-Brunswick","Quebec / Québec","Ontario","Manitoba",
               "Saskatchewan","Alberta","British Columbia / Colombie-Britannique")
#edge <- dplyr::select(ocan, PRENAME %in% edgebo)
#edge <- dplyr::select(ocan, PRENAME %in% edgebo)


edge <- ocan[ocan$PRNAME %in% edgeboduo,]

unique(edge$PRNAME)

Manitoba <- edge[edge$PRNAME=="Manitoba",]
Quebec <- edge[edge$PRNAME=="Quebec / Québec",]


# write_sf(Quebec,"quebec.shp")
# write_sf(Quebec,"quebec.kml")

#write_sf(edge,"canprovs.kml")

# did not work ggplot() +  geom_sf(data = edge) + coord_sf(crs = st_crs(4326))

### Lower lat OF THE CLIP  (we are excluding)
clipprov <- function (prov, upper_lat = 49.75) {
 ## Here  upper_lat is the upper latitude of the included section. 
  prov <- st_transform(prov,crs=4326)
  cat(upper_lat)
  smallerpoly <- st_crop(prov, c(xmin=-124.8, xmax=-66.5, ymin=41.5, ymax=upper_lat))
  return(smallerpoly)
}




## Edbgubo DUO
Manitoba <- edge[edge$PRNAME=="Manitoba",]

Quebec <- edge[edge$PRNAME=="Quebec / Québec",]

NewBrunswick <- edge[edge$PRNAME=="New Brunswick / Nouveau-Brunswick",]

Ontario <- edge[edge$PRNAME=="Ontario",]

Saskatchewan <- edge[edge$PRNAME=="Saskatchewan",]
Alberta <- edge[edge$PRNAME=="Alberta",] 

BritishCol <- edge[edge$PRNAME=="British Columbia / Colombie-Britannique",]


### Province only (* poly lines too long)
#edgebo <- c("New Brunswick","Quebec","Ontario","Manitoba","Saskatchewan","Alberta","British Columbia")

# Manitoba <- edge[edge$PRENAME=="Manitoba",]
# Quebec <- edge[edge$PRENAME=="Quebec",]
# NewBrunswick <- edge[edge$PRENAME=="New Brunswick",]
# Ontario <- edge[edge$PRENAME=="Ontario",]
# Saskatchewan <- edge[edge$PRENAME=="Saskatchewan",]
# Alberta <- edge[edge$PRENAME=="Alberta",] 
# BritishCol <- edge[edge$PRENAME=="British Columbia",]

## Make the bottom-half clips of each Canadian province

Bec <- clipprov(Quebec)
Wick <- clipprov(NewBrunswick)
Rio <- clipprov(Ontario)
Chewan <- clipprov(Saskatchewan)
Berta <- clipprov(Alberta)
Toba <- clipprov(Manitoba)
Lumbia <- clipprov(BritishCol)



Bec <- clipprov(Quebec, upper_lat = 55.0)
Wick <- clipprov(NewBrunswick, upper_lat = 55.0)
Rio <- clipprov(Ontario, upper_lat = 55.0)
Chewan <- clipprov(Saskatchewan, upper_lat = 55.0)
Berta <- clipprov(Alberta, upper_lat = 55.0)
Toba <- clipprov(Manitoba, upper_lat = 55.0)
Lumbia <- clipprov(BritishCol, upper_lat = 55.0)


plot(st_geometry(Rio))

# Bec <- st_union(Bec)
# Wick <- st_union(Wick)
# Lumbia <- st_union(Lumbia)

# mostest <- rbind(Wick,Bec,Lumbia)

# write_sf(mostest, "~/Downloads/Canadaseas.shp")

can7 <- rbind(Berta,Lumbia,Toba,Wick,Rio,Bec,Chewan)
write_sf(can7,"ocanada.shp")

can7 <- rbind(Berta,dryLumbia,Toba,dryWick,dryRio,Bec,Chewan)
write_sf(can7,"ocanada1.shp")

canprov <- rbind(Berta,Lumbia,Toba,Wick,Rio,Bec,Chewan)
write_sf(canprov,"SoCanada.shp")

canprovWest <- rbind(Berta,Lumbia,Toba,Chewan)
write_sf(canprovWest,"SoCanadaWest.shp")

canprovEast <- rbind(Bec,Wick)
write_sf(canprovEast,"SoCanadaEast.shp")

plot(st_geometry(Toba))
plot(st_geometry(Bec))

plot(st_geometry(Rio))

plot(st_geometry(Lumbia))


### Ok now we are working with Rio
### https://www.sciencebase.gov/catalog/item/530f8a0ee4b0e7e46bd300dd
LakePath <- "SomeGreatLakes/"

SuperLake <- read_sf(paste0(LakePath,"hydro_p_LakeSuperior.shp"),stringsAsFactors = FALSE)
st_crs(SuperLake)  ## the lakes are 4369
SuperLake <- st_transform(SuperLake, crs=4326)

Huron <- read_sf(paste0(LakePath,"hydro_p_LakeHuron.shp"),stringsAsFactors = FALSE)
H1 <- st_union(st_transform(Huron, crs=4326))

LakeOntario <- read_sf(paste0(LakePath,"hydro_p_LakeOntario.shp"),stringsAsFactors = FALSE)
O1 <- st_union(st_transform(LakeOntario, crs=4326))

Erie <- read_sf(paste0(LakePath,"hydro_p_LakeErie.shp"),stringsAsFactors = FALSE)
E1 <- st_union(st_transform(Erie, crs=4326))

plot(SuperLake, add=TRUE)

S1 <- st_union(SuperLake)

# plot(S1)
dryRio <- st_difference(Rio,S1)

# plot(st_geometry(dryRio))

dryRio <-st_difference(dryRio,H1)

# plot(st_geometry(dryRio))

dryRio <-st_difference(dryRio,O1)

# plot(st_geometry(dryRio))

dryRio <-st_difference(dryRio,E1)

# plot(st_geometry(dryRio))

singleRio <- st_union(dryRio)

# plot(st_geometry(singleRio))

# plot(st_geometry(O1),add=TRUE)

write_sf(dryRio,"RioCanadaDry.shp")

write_sf(singleRio,"RioCanada1.shp")



### BC Pacific Ocean

dataset <- st_read('ocean/lhy_000h16a_e.shp',
                   stringsAsFactors = FALSE)

str(dataset)

BC_seas <- dataset %>%
  filter(HYDROUID %in% c("255429","372687","467753","8200053",
                         "401999","379613","235633","255430"))

# plot(st_geometry(BC_seas))

pacific <- clipprov(BC_seas)


# plot(st_geometry(pacific))

# plot(st_geometry(pacific1))

pacific1 <- st_union(pacific)

write_sf(pacific,"pacific_clip.shp")

write_sf(pacific1,"pclip1.shp")

dryBC <- st_difference(Lumbia,pacific1)
dryLumbia <- dryBC

write_sf(dryBC, "dryBC.shp")


### Brunswiick Atlantic Ocean 

NewBrunseas <- dataset %>%    ## "8201412" atlantic
  filter(HYDROUID %in% c("7714368","8205527","6286890",
                         "6566044","6765876","7150841","8200077","8065668"))

plot(st_geometry(NewBrunseas))

atlantic <- clipprov(NewBrunseas)

atlantic1 <- st_union(atlantic)

plot(st_geometry(atlantic1))

dryWick <- st_difference(Wick,atlantic1)

write_sf(dryWick, "dryWick.shp")


#### Finish up CSD maps
dryRio <- read_sf("CanadaDryShapes/DryRio.shp")


  
can7 <- rbind(Berta,dryLumbia,Toba,dryWick,dryRio,Bec,Chewan)
write_sf(can7,"ocanada2.shp")




Berta<- st_union(Berta)
dryLumbia<- st_union(dryLumbia)
Toba<- st_union(Toba)
dryWick<- st_union(dryWick)
dryRio<- st_union(dryRio)
Bec<- st_union(Bec)

Chewan<- st_union(Chewan)

can7tall <- rbind(Berta,dryLumbia,Toba,dryWick,dryRio,Bec,Chewan)
write_sf(can7,"ocanada4.shp")


plot(st_geometry(dryRio))
#### Archive some  in-memory objects

library(sf)

write_sf(dryLumbia,"CanadaDryShapes/dryLumbia.shp")
write_sf(Berta,"CanadaDryShapes/Berta.shp")
write_sf(Toba,"CanadaDryShapes/Toba.shp")
write_sf(Chewan,"CanadaDryShapes/Chewan.shp")
write_sf(dryRio,"CanadaDryShapes/dryRio.shp")
write_sf(Bec,"CanadaDryShapes/Bec.shp")
write_sf(dryWick,"CanadaDryShapes/dryLumbia.shp")



