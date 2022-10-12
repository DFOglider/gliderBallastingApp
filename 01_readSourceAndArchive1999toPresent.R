rm(list=ls())
# logical to toggle if the user wants to re-read all archive data (TRUE)
#   or assume that all data previously read is all available and read
#   in only new data.
# note that all src data will always be re-read
reReadData <- TRUE  
library(oce)
library(csasAtlPhys)
library(sp)
data("station2Polygon")
# load in setup file
# check if data has already been read in for speady-ness when reading in new data
outfile <- 'ctd.rda'
filenames <- NULL
if(file.exists(outfile) & !reReadData){
  load(outfile)
  filenamesfull <- unlist(lapply(ctd, function(k) k[['filename']]))
  filenames <- basename(filenamesfull)
  cat(paste('number of ctds already read in', length(ctd)), sep = '\n')
}
# in annual analysis, analysisYear is defined in 00_setupFile.R
#   but for here we'll just assume that the current year is the
#   year we want to look for data.
analysisYear <- as.numeric(format(Sys.Date(), '%Y')) 
years <- 1999:analysisYear
# define path to archive
arcPath <- '\\\\ent.dfo-mpo.ca/ATLShares/Science/BIODataSvc/ARC/Archive/ctd'

# define and read in files that document where to look for data
# from the various programs in the src

srcFile <- paste('./srcMissionLists',
                 c('sourceList.dat', # sigma t - and other misc missions
                   'winterGroundfish/sourceYearMission.dat',
                   'summerGroundfish/listOfSourceMissions.dat',
                   'ctdTransects/sourceList.dat'),
                 sep = '/')

srcMissions <- lapply(srcFile, function(k) read.table(k, header = TRUE, stringsAsFactors = FALSE))
# all source info files have the same headers, so don't need to subset
srcMissions <- do.call('rbind', srcMissions)
# first check if data is in the archive
# going to go through every year, gets too complicated if we don't have all the mission numbers
arcfiles <- NULL
for(y in years){
  cat(paste('Reading in year :', y), sep = '\n')
  path <- paste(arcPath, y, sep = '/')
  files <- list.files(path, pattern = '^CTD_\\w+_\\w+_\\w+_DN\\.ODF$', full.names = TRUE)
  cat(paste('     Found', length(files), 'files.'), sep = '\n')
  if(!is.null(filenames)){
    if(any(filenames %in% basename(files))){
      cat('      Files from this year have already been read, proceeding.', sep = '\n')
      next
    }
  }
  d <- lapply(files, read.ctd.odf)
  lon <- unlist(lapply(d, function(k) k[['longitude']][1]))
  lat <- unlist(lapply(d, function(k) k[['latitude']][1]))
  pip <- point.in.polygon(point.x = lat,
                          point.y = lon,
                          pol.x = station2Polygon$latitude,
                          pol.y = station2Polygon$longitude)
  ok <- pip > 0
  if(any(ok == TRUE)){
    cat(paste('       Found ', length(files[ok]), 'files.'), sep = '\n')
    arcfiles <- c(arcfiles, files[ok])
  }
}

arcMissions <- arcctds <- NULL
if(!is.null(arcfiles)){
  arcctds <- lapply(arcfiles, read.ctd.odf)
  arcFileNames <- unlist(lapply(arcfiles, function(k) tail(strsplit(k, split = '/')[[1]], 1)))
  arcMissionsAll <- gsub('^CTD_(\\w+)_\\w+_\\w+_DN\\.ODF$', '\\1', arcFileNames)
  arcMissions <- unique(arcMissionsAll)
} 

if(!is.null(filenames)){
  arcMissions <- c(arcMissions,
                   unique(gsub('^CTD_(\\w+)_\\w+_\\w+_DN\\.ODF$', 
                               '\\1', 
                               unlist(lapply(ctd, function(k) basename(k[['filename']]))))))
  arcMissions <- arcMissions[-grep('.*\\.ODF', arcMissions)]
}

# note this is up to the user to maintain the  srcFile.
#   it is not necessary for the user to remove the entry into the srcFile
#   once data has been migrated to the archive.

inarcMissions <- unlist(apply(srcMissions, 1, function(k) k[['mission']] %in% arcMissions))
if(!all(inarcMissions)){
  files <- as.vector(unlist(apply(srcMissions[!inarcMissions, ], 1, function(k) list.files(path = k[['path']],
                                                                                                  pattern = k[['pattern']],
                                                                                                  full.names = TRUE))))
  if(!is.null(filenames)){
    # always re-read ALL SRC data
    insrc <- grepl('BIODataSvc\\\\SRC', filenamesfull)
    ctd <- ctd[!insrc]
    cat(paste('      number of ctds after removing src', length(ctd)), sep = '\n')
    #ctd <- ctd[!filenames %in% basename(files)]
  }
  cat(paste('Found', length(files), 'files in the src.'), sep = '\n')
  ctds <- lapply(files, read.ctd.odf)
  lon <- unlist(lapply(ctds, function(k) k[['longitude']][1]))
  lat <- unlist(lapply(ctds, function(k) k[['latitude']][1]))
  okctds <- point.in.polygon(point.x = lon,
                             point.y = lat,
                             pol.x = station2Polygon[['longitude']],
                             pol.y = station2Polygon[['latitude']]) != 0
  srcctds <- ctds[okctds]
  cat(paste('      ', 'found', length(srcctds), 'in src.'), sep = '\n')
}

# check if ctd exists
if(exists('ctd')){
  oldctd <- ctd
}
# combine arc and src
# I think this is the way i'll have to go
# three scenarios
# 1. !is.null(arcctds)  & is.null(srcctds)
# 2. is.null(arcctds) & !is.null(srcctds)
# 3. !is.null(arcctds) & !is.null(srcctds)
if(!is.null(arcctds) & is.null(srcctds)){
  ctd <- arcctds
}
if(is.null(arcctds) & !is.null(srcctds)){
  ctd <- srcctds
}
if(!is.null(arcctds) & !is.null(srcctds)){
  ctd <- c(arcctds, srcctds)
}

if(exists('oldctd')){
  ctd <- c(oldctd, ctd)
}

if(!reReadData) cat(paste('      number of ctds after reading in src and combining with arc data', length(ctd)), sep = '\n')


# trim profiles 
#ctd <- lapply(ctd, ctdTrim) # causing R to abort for some reason ?!
# handle flags, if there are any
ctd <- lapply(ctd, function(k) if(length(k[['flags']]) != 0){handleFlags(k, flags = 2:4)} else {k})

# order ctd by startTime
startTime <- as.POSIXct(unlist(lapply(ctd, function(k) k[['startTime']])), origin = '1970-01-01', tz = 'UTC')
o <- order(startTime)
ctd <- ctd[o]


save(ctd, file = outfile)
