library(dplyr)
#library(terra)
library(sf)


# sapply(c('/srv/shiny-server/cuencas/', 'N:/Mi unidad/git/cuencas'),
#        FUN = function(x) tryCatch(setwd(x), error = function(e) NULL))
 # setwd('/srv/shiny-server/cuencas/')
 # setwd('N:/Mi unidad/git/cuencas')

# qlPts <- sf::read_sf('qlStationsn1404.shp')
# dim(qlPts)
# head(qlPts)
# qlPts$newx <- as.numeric(do.call(rbind, strsplit(qlPts$geometry, ','))[, 1])
# qlPts$newy <- as.numeric(do.call(rbind, strsplit(qlPts$geometry, ','))[, 2])
# save(qlPts, file = 'qlPts_1404.RData')
# qlPts$validated <- 0

#(load('qlPts_1404.RData')) # qlPts
# qlPts$id2 <- gsub('xyz', '-', qlPts$qlid)
# # table(bas$bastype)
# table(qlPts$sou)

#table(qlPts$sou)
# (load('bas_bind5307u1352_snip1724_nosnp1704_hort1879.RData')) # bas
# dim(bas)
# bas <- bas[bas$bastype %in% c('hort', 'nosn'), ]
# dim(bas)
# head(bas@data)
# basins <- st_as_sf(bas)
# basins <- sf::st_transform(basins, crs = sf::st_crs('EPSG:4326'))
# # basins <- spTransform(bas, CRSobj = CRS('EPSG:4326'))
# basins$qlid <- gsub('basin[0-9]_|basin[0-9][0-9]_', '', basins$full)
# #basins[, c('source', 'localid')] <- do.call(rbind, strsplit(basins$qlid, 'xyz'))[, 1]
# basins$source <- do.call(rbind, strsplit(basins$qlid, 'xyz'))[, 1]
# basins$localid <- do.call(rbind, strsplit(basins$qlid, 'xyz'))[, 2]
# basins$id2 <- gsub('xyz', '-', basins$qlid)
# table(basins$bastype)

# save(basins, file = 'SF_wgs_bas_nosnp1704_hort1879.RData')
 (load('rdata/SF_wgs_bas_nosnp1704_hort1879.RData')) # basins
# table(basins$bastype)
# table(basins$source)
# basins$id2
# 
# basAreas <- as.data.frame.matrix( xtabs(data = basins, km2 ~ id2 + bastype) )
# basAreas$qlid <- rownames(basAreas)
# #colnames(basAreas) <- c('qlid', 'bastype', 'km2')
# head(basAreas)
# dim(basAreas)
# 
# basPols <- as.data.frame.matrix(table(basins$id2, basins$bastype))
# basPols$qlid <- rownames(basPols)
# head(basPols)
# #colnames(basPols) <- c('qlid', 'bastype', 'npols')
# 
# #basStatsA <- merge(bas)
# 
# pos <- match(qlPts$id2, basPols$qlid)
# qlPts$npolsHort[which(!is.na(pos))] <- basPols$hort[na.omit(pos)]
# qlPts$npolsNosn[which(!is.na(pos))] <- basPols$nosn[na.omit(pos)]
# 
# pos <- match(qlPts$id2, basAreas$qlid)
# qlPts$km2Hort[which(!is.na(pos))] <- basAreas$hort[na.omit(pos)]
# qlPts$km2Nosn[which(!is.na(pos))] <- basAreas$nosn[na.omit(pos)]
# 
# qlPts$differencekm2 <- abs(qlPts$km2Hort - qlPts$km2Nosn)
# qlPts$differenceperc <- 100* (qlPts$km2Hort / qlPts$km2Nosn)
#  head(as.data.frame(qlPts))
#  qlPts$id <- gsub('.+xyz', '', qlPts$qlid)
#  qlPts$souCol <- as.numeric(as.factor(qlPts$sou))
#  table(qlPts$souCol)
#  table(qlPts$sou)
# qlPts$revised <- qlPts$validated <- 0 
# save(qlPts, file = 'qlPts_1404.RData')
 
 
# head(qlSummary)
# dim(qlSummary)
# unique(qlSummary$cod)
# tail(unique(qlSummary$cod))
#tail(qlSummary$cod, 100)

(load('rdata/qlPts_1404.RData')) # qlPts
(load('rdata/qlSummary_qlList_1240stations.RData')) # "qlSummary" "qlList"
(load('rdata/newPoints_horton_990.RData')) # newPoints
newPoints$qlid <- gsub('xyz', '-', newPoints$id)

newComments <- list.files(path = 'comm/', pattern = '.csv')
revStations <- gsub('.+__|.csv', '', newComments)
qlPts$revised[qlPts$id2 %in% revStations] <- 1


# dim(newPoints) # 990   
# head(newPoints) # 990   


# dim(qlPts) # 1404
# 
# head(qlPts)

# class(bas)
# head(bas@data)

# qlPts$havebas <- qlPts$qlid %in% bas$qlid
# sum(qlPts$qlid %in% bas$qlid)

# selBas <- bas[bas$qlid %in% qlPts$qlid, ]
# dim(selBas)
# 
# allzips <- readRDS("data/superzip.rds")
# allzips$latitude <- jitter(allzips$latitude)
# allzips$longitude <- jitter(allzips$longitude)
# allzips$college <- allzips$college * 100
# allzips$zipcode <- formatC(allzips$zipcode, width=5, format="d", flag="0")
# row.names(allzips) <- allzips$zipcode
# 
# cleantable <- allzips %>%
#   select(
#     City = city.x,
#     State = state.x,
#     Zipcode = zipcode,
#     Rank = rank,
#     Score = centile,
#     Superzip = superzip,
#     Population = adultpop,
#     College = college,
#     Income = income,
#     Lat = latitude,
#     Long = longitude
#   )


# sudo shiny; cd /home/shiny/connecting-landscapes; git add . ; git commit -m "Change coordiantes()"; git push
# git pull main --rebase --autostash
# sudo chown -R shiny:shiny .
# git stash
# remove before commit, split or lost it
# git pull connectscape |||  git pull --rebase --autostash || git pull origin HEAD
