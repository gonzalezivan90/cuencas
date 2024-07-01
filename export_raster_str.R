library(rgrass)

#source('X:/datproj/scripts/basins_GRASS_config.R')
#source('N:/Mi unidad/src/sdg/basins_GRASS_config.R')

# system2("find", "/usr ! -readable -prune -o -type f -executable -iname 'grass??' -print")
# sudo chown -R shiny:shiny /home/shiny/grassdb
# chmod ugo+rwx . 


loc <- initGRASS( #gisBase = "C:/OSGeo4W64/apps/grass/grass78", gisBase = "C:/Program Files/GRASS GIS 7.8",
  gisBase = '/usr/lib/grass78',
  #gisDbase = "C://grassC" , # F:/grass
  gisDbase = "/home/shiny/grassdb" , # F:/grass
  location = "peco" ,
  mapset = "PERMANENT" ,
  override = T)

alredy <- execGRASS('g.list', mapset = 'PERMANENT', type = 'all', pattern = '*', intern = TRUE)
(strgrass <- execGRASS('g.list', mapset = 'PERMANENT', type = 'all', pattern = 'str*', intern = TRUE))

(strgrass <- execGRASS('g.list', mapset = 'PERMANENT', type = 'all', pattern = 'str*', intern = TRUE))

tifPath <- '/home/shiny/grassdb/tif/'
dir.create(tifPath)

for ( i in 1:length(strgrass)){ # i = 1
  print(strgrass[i])
  execGRASS('g.region', raster = strgrass[i], Sys_show.output.on.console = FALSE)
  execGRASS('r.out.gdal', input = strgrass[i], output = paste0(tifPath, strgrass[i]), format="GTiff")
  
  # execGRASS('r.mask', flags = c('r'))
  # execGRASS('r.clip', input = paste0('str_', ptQl$ah), output = paste0(grassname, '_dra_clip'), Sys_show.output.on.console = FALSE)
  strbas <- read_RAST(paste0('dra_', ptQl$ah))
  
}