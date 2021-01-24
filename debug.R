
beams.data<-list()
for(i in beams){
  beams.data[[i]]<-list()
  for(i1 in data.to.write){ 
    beams.data[[i]][[basename(i1)]] <- op$open(sprintf("%s/%s", i, i1) )$read() 
    beams.data[[i]][[basename(i1)]][ beams.data[[i]][[basename(i1)]] > 3.3e+38 ] <-NA
  }
  nn<-beams.data[[i]][["canopy_h_metrics"]]
  nn<-as.list(as.data.frame(t(nn)))
  names(nn)<- sprintf("canopyH_%sp", c(25,50,60,70,75,80,85,90,95 ))
  beams.data[[i]][["canopy_h_metrics"]]<-NULL 
  beams.data[[i]] <- c(beams.data[[i]], nn)
}