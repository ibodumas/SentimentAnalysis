library(ggmap)
library(rjson)
library(plyr)

dirt = "C:/Users/R/Project"
twitterHeader = as.data.frame(read.csv(paste0(dirt, "/twitterHeader.csv")))
twitterHeader = as.data.frame(twitterHeader[,2])
names(twitterHeader) = c("header")

#Getting information and subsetting each file
getRecords = function(twitterHeader)
{
  setwd(paste0(dirt, "/files/Nov_9"))
  files = dir(pattern = ".txt", ignore.case = T)
  len = 1000
  for(f in 1:length(files))
  {
    setwd(paste0(dirt, "/files/Nov_9"))
    file.Name = files[f]
    scan = readLines(file.Name)
    loop = length(scan) %/% len
    remain = length(scan) %% len

    if(remain > 0)
      loop = loop + 1
    
    setwd(paste0(dirt, "/files"))
    
    for(h in 1:loop)
    {
      start = (h-1) * len + 1
      end = h * len
      
      if(remain > 0 & h == loop)
        end = start + remain - 1
      
      dt = ""
      dt = data.frame(sn=start:end)
      for(i in 1:length(twitterHeader$header))
      {
        dt[[paste(twitterHeader$header[i])]] = ""
      }
      
      u = 1
      
      for(i in start:end)
      {
        a = tryCatch(scan[i], error = function(c) "NA")
        b = tryCatch(fromJSON(a), error = function(c) "NA")
        c = tryCatch(unlist(b, recursive = T, use.names = T), error = function(c) "NA")

        #Remove none US
        checkCountry = tryCatch(c[["place.country_code"]], error = function(c) "NA")
        if(toupper(checkCountry) != "US")
        {
          next
        }
        
        #Remove none English
        checkLang = tryCatch(c[["lang-id-code"]], error = function(c) "NA")
        if(toupper(checkLang) != "EN")
        {
          next
        }
        
        txt = tryCatch(c[["text"]], error = function(c) "NA")
        place = tryCatch(c[["place.full_name"]], error = function(c) "NA")
        
        dt[[u, "text"]] = txt
  
        #Split bewteen County and State
        split = tryCatch(strsplit(place, ", "), error = function(c) place)
        
        if(place != split)
        {
          county = tryCatch(split[[1]][1], error = function(c) place)
          state = tryCatch(split[[1]][2], error = function(c) place)
          dt[[u, "county"]] = county
          dt[[u, "state"]] = state
        }
        else
        {
          dt[[u, "county"]] = place
          dt[[u, "state"]] = place
        }
        
        u = u + 1
      }
      dt[dt==""] = NA
      dt = na.omit(dt)
      write.table(dt[,-1], paste0(file.Name,"_", h, "_", loop, ".txt"))
    }
  }
}

start.time <- Sys.time()
record.Explo = getRecords(twitterHeader)
end.time = Sys.time()
time.taken <- end.time - start.time

#Joing the subseted files
joinRecords = function()
{
  setwd(paste0(dirt, "/files"))
  files = dir(pattern = ".txt", ignore.case = T)
  #tb = read.table(files[1])
  
  len = 1000
  loop = length(files) %/% len
  remain = length(files) %% len
  
  if(remain > 0)
    loop = loop + 1
  
  for(h in 1:loop)
  {
    tb = NA
    start = (h-1) * len + 2
    end = h * len + 1
    
    if(remain > 0 & h == loop)
      end = start + remain - 2
    
    start.time = Sys.time()
    
    for(f in start:end)
    {
      file.Name = files[f]
      newtb = tryCatch(read.table(file.Name), error = function(c) "NA")
      if(is.na(newtb) || newtb == "NA")
        next
      
      tb = rbind(tb, newtb)
    }
    
    end.time = Sys.time()
    time.taken.join = end.time - start.time
    
    write.table(tb, paste0(start,"_", end, "_", loop, ".txt2"))
  }
}

explo.Merge = joinRecords()

#Merging to get the final file
setwd(paste0(dirt, "/files"))
files.Final = dir(pattern = ".txt2", ignore.case = T)

finalFile = rbind(read.table(files.Final[1]), read.table(files.Final[2]), read.table(files.Final[3]), read.table(files.Final[4])) 
finalFile = na.omit(finalFile)
write.table(finalFile, "nov9twitter")

twit.data = read.table("nov9twitter")
