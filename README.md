# Assignment1
Coursera R Language John Hopkins R Language pollutantmean all three parts.
Pollutantmean Listed:
pollutantmean <- function(directory,pollutant="sulfate",id=1:332) 
{
  files_list <- list.files(directory,full.names=TRUE)
  dtfr <- data.frame() # created an empty data frame to read all the files togather
  for (i in id)
    {
  dtfr <- rbind(dtfr,read.csv(files_list[i]))
  dtfr_zoomed <- data.frame()  
    }
  dim(dtfr)
  for(f in id)
  {
  if(pollutant=="sulfate")
      {
        get <- dtfr[dtfr$ID==f,2]
      } 
    else
      {
        get <- dtfr[dtfr$ID==f,3] 
      }  
     dtfr_zoomed <- c(get,dtfr_zoomed)
  }
  m <- as.numeric(dtfr_zoomed)
  avg <- mean(m,na.rm=TRUE)
  print(avg)
}
pollutantmean("specdata","sulfate",1:10)
pollutantmean("specdata","nitrate",70:72)
pollutantmean("specdata","nitrate",23)



Function for number of observations listed:
complete <- function(directory, id = 1:332){
  files1_list <- list.files(directory, full.names=TRUE)
  dat <- data.frame()       
  net <- data.frame()       
  for (i in id) 
  {                                
       dat <- rbind(dat, read.csv(files1_list[i]))
  }
  new = data.frame("id" = id,"nobs"=numeric(length(id)))
  c = 1
  for(f in id) 
  {
    good<-complete.cases(dat[dat$ID==f,])
    new[c,2] <-length(good[good=="TRUE"])
    c <- c + 1
  }
  print(new)
}
complete("specdata", 1)
complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 3)



Function for Correlation:
corr <- function(directory, threshold = 0) 
  {
   corrsNum <- numeric(0)
   nobsDfr <- complete("specdata")
   nobsDfr <- nobsDfr[nobsDfr$nobs > threshold, ]
    for (cid in nobsDfr$id) {
    monDfr <- getmonitor(cid, directory)
    corrsNum <- c(corrsNum, cor(monDfr$sulfate, monDfr$nitrate, use = "pairwise.complete.obs"))
  }
  return(corrsNum)
}
complete <- function(directory, id = 1:332) {
  nobsNum <- numeric(0)
  for (cid in id) {
  cDfr <- getmonitor(cid, directory)
  nobsNum <- c(nobsNum, nrow(na.omit(cDfr)))
  }
  data.frame(id = id, nobs = nobsNum)
}

getmonitor <- function(id, directory, summarize = FALSE) {
  fileStr <- paste(directory, "/", sprintf("%03d", as.numeric(id)), ".csv", 
                   sep = "")
  rawDfr <- read.csv(fileStr)
if (summarize) {
    print(summary(rawDfr))
  }
  return(rawDfr)
}
cr <- corr("specdata", 150)
head(cr)
summary(cr)
cr <- corr("specdata", 400)
head(cr)
summary(cr)
cr <- corr("specdata", 5000)
summary(cr)
length(cr)
cr <- corr("specdata")
summary(cr)
length(cr)
