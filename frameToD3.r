load("sampleData")
#My sample data is 1000 rows, with the first column a unique id, followed by 100 columns of data variables
groupVars <- c("path")
dataVars <-  colnames(dt)[!colnames(dt) %in% groupVars]
outfile
frameToJSON <- function(dt,groupVars,dataVars,outfile){
  #packages we will need:
  require(data.table)
  require(RJSONIO)
  
  #Here you may want to sort by colSums() to keep only the most relevant variables.
  
  #calculate the correlation matrix
  t <- cor(dt[,c(!colnames(dt) %in% groupVars),with=F])
  
  #calculate the hierarchical cluster structure from the correlation scores
  hc <- hclust(dist(t), "ward")
  
  #take a look at what your strucutre:
  plot(hc)
  
  #now we split the data based on membership structure. We will take four levels:
  #(basically this means we will calculate which group each variable belongs in for different levels of the tree strucutre)
  memb2 <- as.character(cutree(hc, k = 2))
  memb6 <- as.character(cutree(hc, k = 6))
  memb15 <- as.character(cutree(hc, k = 15))
  memb40 <- as.character(cutree(hc, k = 40))
  
  #Now put this information into a table, together with the labels and the order in which they should appear:
  b=data.table(memb2,memb6,memb15,memb40,label=hc$labels,order=hc$order)
  
  #We might want to know the size of each node. Let's add that
  b$size <- colSums(dt[,c(dataVars),with=F])
  
  #sort the data so it alligns with the structure calculated using hclust()
  setkey(b,order)
  #drop the order variable:
  b[,order:=NULL]
  
  #we define a function which will create a nested list in JSON format:
  #From here: http://stackoverflow.com/questions/12818864/how-to-write-to-json-with-children-from-r
  makeList<-function(x){
    if(ncol(x)>2){
      listSplit<-split(x[-1],x[1],drop=T)
      lapply(names(listSplit),function(y){list(name=y,imports=makeList(listSplit[[y]]))})
    }else{
      lapply(seq(nrow(x[1])),function(y){list(name=x[,1][y],size=x[,2][y])})
    }
  }
  
  #This will not work on a data.table
  
  b <- data.frame(b)
  out <- makeList(b)
  #Have a look at the structure this creates:
  print (head(out))
  
  #Basically we have made a list of lists containing the information from the tree diagram.
  #Finally we put everythin into a list, convert this to json format and save it as data.json
  jsonOut<-toJSON(list(name="Centre",children=makeList(b)))
  
  #We use the cat function here, because in some cases you may want to add separators, or a prefix and suffix to make the formatting just right
  cat(jsonOut,file=outfile)
}

frameToJSON(dt,groupVars,dataVars,outfile="data.json")