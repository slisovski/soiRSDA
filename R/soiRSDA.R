##' Title
##'
##' ...
##'
##'
##' @title title
##'
##' @param y points represented by a two-column matrix or data.frame, or `SpatialPoints`; `SpatialPolygons`; `SpatialLines`; `Extent`; or a numeric vector representing cell numbers
##' @param ...
##' @param path the path leading to the `VHP.G16.C07.SM.csv` file. If `NULL` (default), the funcition will search in default R drive folder.
##' @return ...
##' @export
getData <- function(y, dataset = "VHP", buffer = NULL, dates = NULL, path = NULL, returnRast = TRUE, ...) {


  if(dataset=="VHP") {
  if (is.null(path)) {
    if(file.exists("R:/40 Data/99 Diverse/RemoteSensedDatabse/VHP.G16.C07.SM/VHP.G16.C07.SM.csv")) {
    path <- list.files("R:/40 Data/99 Diverse/RemoteSensedDatabse/", pattern = "VHP.G16.C07.SM.csv", recursive = T, full.names = T)
    } else {
      stop("Can't find VHP.G16.C07.SM.csv or R drive on your system.")
    }
  } else {
    if(!file.exists(path)) stop("Path is not leading to an existing file.")
  }
    data(RasterIndex_VHP)
    data(DateIndex_VHP)
  }
  if(dataset=="Tmax") {
    if (is.null(path)) {
      if(file.exists("R:/40 Data/99 Diverse/RemoteSensedDatabse/CPC_GDT/CPC_GDT_tmax.csv")) {
        path <- list.files("R:/40 Data/99 Diverse/RemoteSensedDatabse/", pattern = "CPC_GDT_tmax.csv", recursive = T, full.names = T)
      } else {
        stop("Can't find CPC_GDT_tmax.csv or R drive on your system.")
      }
    } else {
      if(!file.exists(path)) stop("Path is not leading to an existing file.")
    }
    data(RasterIndex_CPC)
    data(DateIndex_CPC)
  }



  if((is.matrix(y) | is.data.frame(y)) & !is.null(buffer)) {
    ind <- unlist(raster::extract(RasterIndex, y, buffer = buffer))

  } else {
  ind <- unlist(raster::extract(RasterIndex, y))
  }
  ind <- ind[!is.na(ind)]

  if(!is.null(dates)) {
    ind2 <- which(DateIndex>=as.numeric(as.POSIXct(dates[1])) & DateIndex<as.numeric(as.POSIXct(dates[2])))
    data <- as.matrix(fread(path, select = ind, skip = min(ind2)-1, nrows = length(ind2), header = TRUE))
  } else {
    ind2 <- 1:length(DateIndex)
    data <- as.matrix(fread(path, select = ind, header = TRUE))
  }

  if(!returnRast) {
    colnames(data)  <- c(1:ncol(data))
    row.names(data) <- DateIndex[ind2]
  } else {

    crds0 <- coordinates(RasterIndex)[!is.na(RasterIndex[]),][ind,]

  rastTmp <- RasterIndex;
  rastTmp <- crop(rastTmp, extent(c(range(crds0[,1]), range(crds0[,2]))), snap = "out")
    rastTmp[] <- ifelse(rastTmp[]%in%ind, rastTmp[], NA)

  ind3    <- match(rastTmp[], ind)

  out <- stack()
  for(i in 1:nrow(data)) {
    rastTmp[which(!is.na(ind3))] <- as.numeric(data[i,])
    out <- stack(out, rastTmp)
  }
  names(out) <- paste0("X_", format(as.POSIXct(DateIndex[ind2], origin = "1970-01-01"), "%y-%m-%d"))
  data <- out
  }

return(data)
}
