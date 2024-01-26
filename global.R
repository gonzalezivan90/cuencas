library(dplyr)

library(terra)
library(sf)


setwd('/srv/shiny-server/cuencas/')

(load('bas_bind5307u1352_snip1724_nosnp1704_hort1879.RData')) # bas
(load('qlSummary_qlList_1240stations.RData')) # "qlSummary" "qlList"
#dim(qlSummary)
#length(qlList)

# qlPts <- sf::read_sf('qlStationsn1404.shp')
# dim(qlPts)
# save(qlPts, file = 'qlPts_1404.RData')
# table(bas$bastype)

plot(qlPts)
head(qlPts)

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


# system('sudo shiny; cd /home/shiny/connecting-landscapes; git add . ; git commit -m "Change coordiantes()"; git push')
# git pull main --rebase --autostash
# sudo chown -R shiny:shiny .
# git stash
# remove before commit, split or lost it
# git pull connectscape |||  git pull --rebase --autostash || git pull origin HEAD
