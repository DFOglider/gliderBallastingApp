# note, this isn't a formal function, written just to save space in scripts and to avoid drift
gridClimatologyDaily <- function(d){
  #um <- unlist(lapply(d, function(k) k[['month']]))\
  dtime <- as.POSIXct(unlist(lapply(d, function(k) k[['startTime']])), origin = '1970-01-01', tz = 'UTC')
  dmonth <- as.POSIXlt(dtime)$mon + 1
  dyear <- as.POSIXlt(dtime)$year + 1900
  fakeYear <- unique(dyear)[1] # hoping that i'm smart and keep it to just one year
  um <- unique(dmonth)
  cnt <- 1
  mctd <- vector(mode = 'list')
  # first find december profiles and create ones for year before
  ok <- dmonth == 12
  ddec <- d[ok]
  for(id in 1:length(ddec)){
    fakectd <- ddec[[id]]
    origTime <- as.POSIXlt(dtime[ok][id])
    fakeTime <- as.POSIXct(paste(fakeYear - 1, origTime$mon + 1, origTime$mday, sep = '-'), tz = 'UTC')
    fakectd <- oceSetMetadata(fakectd,
                              'startTime',
                              fakeTime)
    mctd[[cnt]] <- fakectd
    cnt <- cnt + 1
  }
  # next, find january profiles and create ones for year after
  ok <- dmonth == 1
  djan <- d[ok]
  for(id in 1:length(djan)){
    fakectd <- djan[[id]]
    origTime <- as.POSIXlt(dtime[ok][id])
    fakeTime <- as.POSIXct(paste(fakeYear + 1, origTime$mon + 1, origTime$mday, sep = '-'), tz = 'UTC')
    fakectd <- oceSetMetadata(fakectd,
                              'startTime',
                              fakeTime)
    mctd[[cnt]] <- fakectd
    cnt <- cnt + 1
  }
  # combine all the ctds together
  allctd <- c(mctd, d)

  # going to spit out warnings for no longitude/latitude, but that's ok for this purpose
  start <- as.POSIXct(unlist(lapply(allctd, function(k) k[['startTime']])), origin = '1970-01-01', tz = 'UTC')
  o <- order(start)
  allctd <- allctd[o]
  s <- as.section(allctd)
  s@metadata$startTime <- as.POSIXct(unlist(lapply(allctd, function(k) k[['startTime']])), origin = '1970-01-01', tz = 'UTC')
  sg <- sectionGrid(s)
  sg
}
