library(dplyr)
#library(terra)
library(sf)


 # setwd('/srv/shiny-server/cuencas/')
 # setwd('N:/Mi unidad/git/cuencas')

# qlPts <- sf::read_sf('qlStationsn1404.shp')
# dim(qlPts)
# head(qlPts)
# qlPts$newx <- as.numeric(do.call(rbind, strsplit(qlPts$geometry, ','))[, 1])
# qlPts$newy <- as.numeric(do.call(rbind, strsplit(qlPts$geometry, ','))[, 2])
# save(qlPts, file = 'qlPts_1404.RData')

(load('qlPts_1404.RData')) # qlPts
# table(bas$bastype)
qlPts$id2 <- gsub('xyz', '-', qlPts$qlid)


(load('bas_bind5307u1352_snip1724_nosnp1704_hort1879.RData')) # bas
(load('qlSummary_qlList_1240stations.RData')) # "qlSummary" "qlList"
(load('newPoints_horton_990.RData')) # newPoints
newPoints$qlid <- gsub('xyz', '-', newPoints$id)
# dim(newPoints) # 990   
# head(newPoints) # 990   


# dim(qlPts) # 1404
# 
# head(qlPts)
# head(bas@data)
bas$qlid <- gsub('basin[0-9]_|basin[0-9][0-9]_', '', bas$full)
#bas[, c('source', 'localid')] <- do.call(rbind, strsplit(bas$qlid, 'xyz'))[, 1]
bas$source <- do.call(rbind, strsplit(bas$qlid, 'xyz'))[, 1]
bas$localid <- do.call(rbind, strsplit(bas$qlid, 'xyz'))[, 2]
bas$id2 <- gsub('xyz', '-', bas$qlid)

class(bas)
head(bas@data)

# qlPts$havebas <- qlPts$qlid %in% bas$qlid
# sum(qlPts$qlid %in% bas$qlid)

# selBas <- bas[bas$qlid %in% qlPts$qlid, ]
# dim(selBas)

allzips <- readRDS("data/superzip.rds")
allzips$latitude <- jitter(allzips$latitude)
allzips$longitude <- jitter(allzips$longitude)
allzips$college <- allzips$college * 100
allzips$zipcode <- formatC(allzips$zipcode, width=5, format="d", flag="0")
row.names(allzips) <- allzips$zipcode

cleantable <- allzips %>%
  select(
    City = city.x,
    State = state.x,
    Zipcode = zipcode,
    Rank = rank,
    Score = centile,
    Superzip = superzip,
    Population = adultpop,
    College = college,
    Income = income,
    Lat = latitude,
    Long = longitude
  )


# sudo shiny; cd /home/shiny/connecting-landscapes; git add . ; git commit -m "Change coordiantes()"; git push
# git pull main --rebase --autostash
# sudo chown -R shiny:shiny .
# git stash
# remove before commit, split or lost it
# git pull connectscape |||  git pull --rebase --autostash || git pull origin HEAD
