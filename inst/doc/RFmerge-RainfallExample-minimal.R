## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----Install_Stable, eval = FALSE---------------------------------------------
#  install.packages("RFmerge")

## ----Install_Unstable, eval = FALSE-------------------------------------------
#  if (!require(devtools)) install.packages("devtools")
#  library(devtools)
#  install_github("hzambran/RFmerge")

## ----Loading_other_pks, eval = TRUE, message=FALSE----------------------------
library(zoo)
library(sf)
library(rgdal)
library(raster)

## ----LoadingRFmerge, eval = TRUE, message=FALSE-------------------------------
library(RFmerge)

## ----Loading_GroundObservarions, eval = TRUE----------------------------------
data(ValparaisoPPts)    
data(ValparaisoPPgis) 
data(ValparaisoSHP)  

## ----LoadingSatelliteData, eval = TRUE----------------------------------------
chirps.fname   <- system.file("extdata/CHIRPS5km.tif",package="RFmerge")
prsnncdr.fname <- system.file("extdata/PERSIANNcdr5km.tif",package="RFmerge")
dem.fname      <- system.file("extdata/ValparaisoDEM5km.tif",package="RFmerge")

CHIRPS5km        <- brick(chirps.fname)
PERSIANNcdr5km   <- brick(prsnncdr.fname)
ValparaisoDEM5km <- raster(dem.fname)   

## ----GivingMeaninfulNamesToLayers, eval = TRUE--------------------------------
#ldates                <- hydroTSM::dip("1983-01-01", "1983-08-31")
ldates                <- seq(from=as.Date("1983-01-01"), to=as.Date("1983-08-31"), by="days")
names(CHIRPS5km)      <- ldates
names(PERSIANNcdr5km) <- ldates  

## ----ExploringMetadata--------------------------------------------------------
head(ValparaisoPPgis)

## ----PlottingPts, fig.width = 7, fig.height = 3.5, fig.align = "center", message=FALSE----
main <- paste("Daily precipitation for the station", ValparaisoPPgis$Code[1])
ylab <- "Precipitation [mm]"
x.ts <- ValparaisoPPts[,1]

#hydroTSM::hydroplot(x.ts, pfreq="o", main=main, ylab= ylab)
plot(x.ts, main=main, ylab= ylab, col="blue")
grid()

## ----PlotingTotalP, fig.width= 4, fig.height = 4, fig.align = "center", message=FALSE----
chirps.total   <- sum(CHIRPS5km, na.rm= FALSE)
persiann.total <- sum(PERSIANNcdr5km, na.rm= FALSE)


plot(chirps.total, main = "CHIRPSv2 [Jan - Aug] ", xlab = "Longitude", ylab = "Latitude")
plot(ValparaisoSHP[1], add=TRUE, col="transparent")

plot(persiann.total, main = "PERSIANN-CDR [Jan - Aug]", xlab = "Longitude", ylab = "Latitude")
plot(ValparaisoSHP[1], add=TRUE, col="transparent")

## ----SpatialMetadata----------------------------------------------------------
stations <- ValparaisoPPgis
( stations <- st_as_sf(stations, coords = c('lon', 'lat'), crs = 4326) )

## ----PlottingDEMwithObs, fig.width = 4, fig.height = 4, fig.align = "center"----
plot(ValparaisoDEM5km, main="SRTM-v4", xlab="Longitude", ylab="Latitude", col=terrain.colors(255))
plot(ValparaisoSHP[1], add=TRUE, col="transparent")
plot(stations[1], add=TRUE, pch = 16, col="black")

## ----nLayersVerification------------------------------------------------------
nlayers(CHIRPS5km)
( nlayers(CHIRPS5km) == nlayers(PERSIANNcdr5km) )
( nlayers(CHIRPS5km) == nrow(ValparaisoPPts) )

## ----SpatialExtentVerification------------------------------------------------
extent(CHIRPS5km)
( extent(CHIRPS5km) == extent(PERSIANNcdr5km) )
( extent(CHIRPS5km) == extent(ValparaisoDEM5km) )

## ----SpatialResolutionVerification--------------------------------------------
res(CHIRPS5km)
( res(CHIRPS5km) == res(PERSIANNcdr5km) )
( res(CHIRPS5km) == res(ValparaisoDEM5km) )

## ----ReprojectingRasters------------------------------------------------------
utmz19s.p4s <- CRS("+init=epsg:32719") # WGS 84 / UTM zone 19S

CHIRPS5km.utm        <- projectRaster(from=CHIRPS5km, crs=utmz19s.p4s)
PERSIANNcdr5km.utm   <- projectRaster(from=PERSIANNcdr5km, crs=utmz19s.p4s)
ValparaisoDEM5km.utm <- projectRaster(from=ValparaisoDEM5km, crs=utmz19s.p4s)

## ----ReprojectingMetadata-----------------------------------------------------
stations.utm <- sf::st_transform(stations, crs=32719) # for 'sf' objects

## ----ReprojectingSHP----------------------------------------------------------
ValparaisoSHP.utm <- sf::st_transform(ValparaisoSHP, crs=32719)

## ----FinalMEtadata------------------------------------------------------------
st.coords <- st_coordinates(stations.utm)
lon       <- st.coords[, "X"]
lat       <- st.coords[, "Y"]

ValparaisoPPgis.utm <- data.frame(ID=stations.utm[["Code"]], lon=lon, lat=lat)

## ----CovariatesCreation-------------------------------------------------------
covariates.utm <- list(chirps=CHIRPS5km.utm, persianncdr=PERSIANNcdr5km.utm, 
                       dem=ValparaisoDEM5km.utm)

## ----RFmergeWithoutParallelisation, eval = FALSE------------------------------
#  drty.out <- file.path(tempdir(), "Test.nop")
#  rfmep <- RFmerge(x=ValparaisoPPts, metadata=ValparaisoPPgis.utm, cov=covariates.utm,
#                   id="ID", lat="lat", lon="lon", mask=ValparaisoSHP.utm,
#                   training=0.8, write2disk=TRUE, drty.out=drty.out)

## ----OnWindows, eval = TRUE---------------------------------------------------
onWin <- ( (R.version$os=="mingw32") | (R.version$os=="mingw64") )
ifelse(onWin, parallel <- "parallelWin", parallel <- "parallel")

## ----RFmergeWithLinuxParallelisation, eval = TRUE-----------------------------
par.nnodes <- min(parallel::detectCores()-1, 2)
drty.out <- file.path(tempdir(), "Test.par")
rfmep <- RFmerge(x=ValparaisoPPts, metadata=ValparaisoPPgis.utm, cov=covariates.utm,
                 id="ID", lat="lat", lon="lon", mask=ValparaisoSHP.utm, 
                 training=0.8, write2disk=TRUE, drty.out=drty.out, 
                 parallel=parallel, par.nnodes=par.nnodes)

## ----RFmergeWithWindowsParallelisation, eval = FALSE--------------------------
#  par.nnodes <- min(parallel::detectCores()-1, 2)
#  drty.out <- file.path(tempdir(), "Test.par")
#  rfmep <- RFmerge(x=ValparaisoPPts, metadata=ValparaisoPPgis.utm, cov=covariates.utm,
#                   id="Code", lat="lat", lon="lon", mask=ValparaisoSHP.utm,
#                   training=0.8, write2disk=TRUE, drty.out=drty.out,
#                   parallel=parallelWin, par.nnodes=par.nnodes)

## ----Evaluation, eval= FALSE--------------------------------------------------
#  ts.path       <- paste0(drty.out, "/Ground_based_data/Evaluation/Evaluation_ts.txt")
#  metadata.path <- paste0(drty.out, "/Ground_based_data/Evaluation/Evaluation_metadata.txt")
#  
#  eval.ts  <- read.zoo(ts.path, header = TRUE)
#  eval.gis <- read.csv(metadata.path)
#  
#  # promoting 'eval.gis' into a spatial object (in order to be plotted)
#  ( eval.gis.utm <- st_as_sf(eval.gis, coords = c('lon', 'lat'), crs = 32719) )

## ---- fig.width = 4, fig.height = 4, fig.align = "center", eval= FALSE--------
#  plot(rfmep[[11]], main="RF-MEP precipitation for 1983-01-11", xlab="Longitude", ylab="Latitude")
#  plot(ValparaisoSHP.utm[1], add=TRUE, col="transparent")
#  plot(eval.gis.utm, add=TRUE, pch = 16, col="black")

## ---- fig.width = 4, fig.height = 4, fig.align = "center", eval= FALSE--------
#  rfmep.total <- sum(rfmep, na.rm= FALSE)
#  
#  plot(rfmep.total, main = "RF-MEP [Jan - Aug]", xlab = "Longitude", ylab = "Latitude")
#  plot(ValparaisoSHP.utm[1], add=TRUE, col="transparent")

## ----EvaluationExtraction, eval= FALSE----------------------------------------
#  coordinates(eval.gis) <- ~ lon + lat
#  
#  rfmep.ts    <- t(raster::extract(rfmep, eval.gis))
#  chirps.ts   <- t(raster::extract(CHIRPS5km.utm, eval.gis))
#  persiann.ts <- t(raster::extract(PERSIANNcdr5km.utm, eval.gis))

## ----EvaluationComputation, eval= FALSE---------------------------------------
#  
#  # Defining a function to compute the Nash-Sutcliffe efficiency(NSE)
#  NSE <- function(sim, obs) return( 1 - (sum((obs - sim)^2)/ (sum((obs - mean(obs))^2)) ) )
#  
#  sres       <- list(chirps.ts, persiann.ts, rfmep.ts)
#  nsres      <- length(sres)
#  nstations  <- ncol(eval.ts)
#  tmp        <- rep(NA, nstations)
#  nse.table  <- data.frame(ID=eval.gis[["ID"]], CHIRPS=tmp, PERSIANN_CDR=tmp, RF_MEP=tmp)
#  
#  # Computing the NSE between the observed rainfall measured in each one of the raingauges
#  # of the training dataset and CHIRPSv2, PERSIANN-CDR, the merged product `rfmep`:
#  
#  for (i in 1:nsres) {
#    ldates <- time(eval.ts)
#    lsim   <- zoo(sres[[i]], ldates)
#    nse.table[, (i+1)] = NSE(sim= lsim, obs= eval.ts)
#  } # FOR end

## ----EvaluationPlotting, fig.width=7, fig.height=7, fig.align = "center", eval = FALSE----
#  
#  # Boxplot with a graphical comparison
#  sres.cols <- c("powderblue", "palegoldenrod", "mediumseagreen")
#  boxplot(nse.table[,2:4], main = "NSE evaluation for Jan - Aug 1983",
#          xlab = "P products", ylab = "NSE'", ylim = c(0, 1), # horizontal=TRUE,
#          col=sres.cols)
#  legend("topleft", legend=c("CHIRPS", "PERSIANN-CDR", "RF-MEP"), col=sres.cols, pch=15, cex=1.5, bty="n")
#  grid()

## ----SoftwareDetails, echo=FALSE, eval = TRUE---------------------------------
sessionInfo()$platform
sessionInfo()$R.version$version.string 
paste("RFmerge", sessionInfo()$otherPkgs$RFmerge$Version)

