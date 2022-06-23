
library(Matrix)
library(SparseM)
library(lavaan)
library(data.table)
library(LiblineaR)
library(cluster)
library(ggplot2)

#setwd("C:/Users/ppavl/OneDrive - The University of Memphis/IES Data")
setwd("C:\\Users\\Liang Zhang\\Desktop\\2022_Spring\\ExampleData_LKT")

source("LKTfunctions.R")

datafile0<-"ds1465_tx_All_Data_64_2016_0720_222352.txt"
datafile1<-"as.csv"  # Assistments
datafile2<-"2012-2013-data-with-predictions-4-final.csv"  #
datafile3<-"ds2032_tx_All_Data_114_2017_0207_223411.txt"  #ALEKS
datafile4<-"ds4352_tx_All_Data_6426_2020_0618_064024.txt" #computer programming

########################################## Original Example########################################################
val_0<-read.table(colClasses = c("Anon.Student.Id"="character"),"ds1465_tx_All_Data_64_2016_0720_222352.txt",sep="\t", header=TRUE,quote="\"")
val_0$KC..Default.<-val_0$Problem.Name
# make it a datatable
val_0= setDT(val_0)

#make unstratified folds for crossvaldiations
val_0$fold<-sample(1:5,length(val_0$Anon.Student.Id),replace=T)

# get the times of each trial in seconds from 1970
val_0$CF..Time.<-as.numeric(as.POSIXct(as.character(val_0$Time),format="%Y-%m-%d %H:%M:%S"))

#make sure it is ordered in the way the code expects
val_0<-val_0[order(val_0$Anon.Student.Id, val_0$CF..Time.),]

#create a binary response column to predict and extract only data with a valid value
val_0$CF..ansbin.<-ifelse(tolower(val_0$Outcome)=="correct",1,ifelse(tolower(val_0$Outcome)=="incorrect",0,-1))
val_0<-val_0[val_0$CF..ansbin==0 | val_0$CF..ansbin.==1,]

# create durations
val_0$Duration..sec.<-(val_0$CF..End.Latency.+val_0$CF..Review.Latency.+500)/1000

# this function needs times and durations but you don't need it if you don't want to model time effects
val_0 <- computeSpacingPredictors(val_0, "KC..Default.") #allows recency, spacing, forgetting features to run


val_00<-setDT(read.table("ds1465_tx_All_Data_64_2016_0720_222352.txt",sep="\t",
                      header=TRUE,na.strings="NA",quote="",comment.char = ""))
val_00$CF..ansbin.<-ifelse((val_00$Outcome)=="CORRECT",1,0)
val_00$CF..ansbin.<-as.numeric(val_00$CF..ansbin.)

val_00$fold<-sample(1:5,length(val_00$Anon.Student.Id),replace = T)
val_00$CF..Time.<-as.numeric(as.POSIXct(as.character(val_00$Time),format="%m/%d/%Y %H:%M"))

#The column "$Outcome" and "$KC..Default." is in the original dataset by default
#make sure it is ordered in the way the code expects
val_00<-val_00[order(val_00$Anon.Student.Id, val_00$CF..Time.),]

mnames<-c("SimplePFA",
          "Full PFA",
          "Full PFA full autoKC additive",
          # "Full PFA full autoKC interactive",
          "Simple PFA full autoKC interactive",
          "Full PFA simple autoKC interactive",
          "Simple PFA simple autoKC interactive",
          "Log SimplePFA",
          "Log Full PFA",
          "Log Full PFA full autoKC additive",
          # "Log Full PFA full autoKC interactive",
          "Log Simple PFA full autoKC interactive",
          "Log Full PFA simple autoKC interactive",
          "Log Simple PFA simple autoKC interactive")

cvr2s<-data.frame(name=mnames,cvr2s=NA)
r2s<-data.frame(name=mnames,r2s=NA)

compl<-list(c("Anon.Student.Id","KC..Default.", "KC..Default.", "KC..Default."),
            c("Anon.Student.Id","KC..Default.", "KC..Default.", "KC..Default."),
            c("Anon.Student.Id","KC..Default.", "KC..Default.", "KC..Default." ,"KC..Default." ,"KC..Default."),
            #    c("Anon.Student.Id","KC..Default.", "KC..Default.", "KC..Default." ,"KC..Default." ,"KC..Default."),
            c("Anon.Student.Id","KC..Default.", "KC..Default.", "KC..Default." ,"KC..Default." ,"KC..Default."),
            c("Anon.Student.Id","KC..Default.", "KC..Default.", "KC..Default." ,"KC..Default." ,"KC..Default."),
            c("Anon.Student.Id","KC..Default.", "KC..Default.", "KC..Default." ,"KC..Default." ,"KC..Default."),c("Anon.Student.Id","KC..Default.", "KC..Default.", "KC..Default."),
            c("Anon.Student.Id","KC..Default.", "KC..Default.", "KC..Default."),
            c("Anon.Student.Id","KC..Default.", "KC..Default.", "KC..Default." ,"KC..Default." ,"KC..Default."),
            #    c("Anon.Student.Id","KC..Default.", "KC..Default.", "KC..Default." ,"KC..Default." ,"KC..Default."),
            c("Anon.Student.Id","KC..Default.", "KC..Default.", "KC..Default." ,"KC..Default." ,"KC..Default."),
            c("Anon.Student.Id","KC..Default.", "KC..Default.", "KC..Default." ,"KC..Default." ,"KC..Default."),
            c("Anon.Student.Id","KC..Default.", "KC..Default.", "KC..Default." ,"KC..Default." ,"KC..Default."))
featl<-list(c("intercept","intercept",  "linefail",  "linesuc"),
            c("intercept","intercept",  "linefail$", "linesuc$"),
            c("intercept","intercept",  "linefail$", "linefail$", "linesuc$", "linesuc$"),
            #    c("intercept","intercept",  "linefail$", "linefail$", "linesuc$", "linesuc$"),
            c("intercept","intercept",  "linefail", "linefail$", "linesuc", "linesuc$"),
            c("intercept","intercept",  "linefail$", "linefail", "linesuc$", "linesuc"),
            c("intercept","intercept",  "linefail", "linefail", "linesuc", "linesuc"),
            c("intercept","intercept",  "logfail",  "logsuc"),
            c("intercept","intercept",  "logfail$", "logsuc$"),
            c("intercept","intercept",  "logfail$", "logfail$", "logsuc$", "logsuc$"),
            #    c("intercept","intercept",  "logfail$", "logfail$", "logsuc$", "logsuc$"),
            c("intercept","intercept",  "logfail", "logfail$", "logsuc", "logsuc$"),
            c("intercept","intercept",  "logfail$", "logfail", "logsuc$", "logsuc"),
            c("intercept","intercept",  "logfail", "logfail", "logsuc", "logsuc"))
connl<-list(c("+","+","+","+"),
            c("+","+","+","+"),
            c("+","+","+","+","+","+"),
            #    c("+","+","+","*","+","*"),
            c("+","+","+","*","+","*"),
            c("+","+","+","*","+","*"),
            c("+","+","+","*","+","*"),
            c("+","+","+","+"),
            c("+","+","+","+"),
            c("+","+","+","+","+","+"),
            #    c("+","+","+","*","+","*"),
            c("+","+","+","*","+","*"),
            c("+","+","+","*","+","*"),
            c("+","+","+","*","+","*"))
autol <- list(c(F,F,F,F),
              c(F,F,F,F),
              c(F,F,F,T,F,T),
              #    c(F,F,F,T,F,T),
              c(F,F,F,T,F,T),
              c(F,F,F,T,F,T),
              c(F,F,F,T,F,T),
              c(F,F,F,F),
              c(F,F,F,F),
              c(F,F,F,T,F,T),
              #    c(F,F,F,T,F,T),
              c(F,F,F,T,F,T),
              c(F,F,F,T,F,T),
              c(F,F,F,T,F,T))


for(i in 1:length(compl)){
  modelob <<- LKT(data = val_00,components = compl[[i]],features = featl[[i]],connectors = connl[[i]],autoKC = autol[[i]],
                  cv=TRUE,verbose = FALSE,
                  autocent=4)
  cat(mnames[i]," R2cv =  ",mean(modelob$cv_res$mcfad))
  cat(" R2 =  ",modelob$r2,"\n")

  r2s$r2s[i]<-modelob$r2
  r2s$cvr2s[i]<-mean(modelob$cv_res$mcfad)
}

r2s$r2s<-r2s$r2s-min(r2s$r2s)
r2s$name <- factor(r2s$name,levels = rev(mnames))
plot<-ggplot(r2s,
             aes(name,r2s)) +
  geom_bar(stat = "identity") +
  coord_flip()
plot

r2s$cvr2s<-r2s$cvr2s-min(r2s$cvr2s)
r2s$name <- factor(r2s$name,levels = rev(mnames))
plot<-ggplot(r2s,
             aes(name,cvr2s)) +
  geom_bar(stat = "identity") +
  coord_flip()
plot

# Additive Factors Model (AFM) fixed effect version

modelob <- LKT(
  data = val_00, interc=TRUE,
  components = c("Anon.Student.Id","KC..Default.","KC..Default."),
  features = c("intercept", "intercept", "lineafm$"))



########################################### Example from Assistments ######################################




val_1<-read.csv("as.csv")
val_1<-val_1[1:50000,]


mnames<-c("SimplePFA",
          "Full PFA",
          "Full PFA full autoKC additive",
          # "Full PFA full autoKC interactive",
          "Simple PFA full autoKC interactive",
          "Full PFA simple autoKC interactive",
          "Simple PFA simple autoKC interactive",
          "Log SimplePFA",
          "Log Full PFA",
          "Log Full PFA full autoKC additive",
          # "Log Full PFA full autoKC interactive",
          "Log Simple PFA full autoKC interactive",
          "Log Full PFA simple autoKC interactive",
          "Log Simple PFA simple autoKC interactive")

cvr2s<-data.frame(name=mnames,cvr2s=NA)
r2s<-data.frame(name=mnames,r2s=NA)

compl<-list(c("Anon.Student.Id","KC..Default.", "KC..Default.", "KC..Default."),
            c("Anon.Student.Id","KC..Default.", "KC..Default.", "KC..Default."),
            c("Anon.Student.Id","KC..Default.", "KC..Default.", "KC..Default." ,"KC..Default." ,"KC..Default."),
            #    c("Anon.Student.Id","KC..Default.", "KC..Default.", "KC..Default." ,"KC..Default." ,"KC..Default."),
            c("Anon.Student.Id","KC..Default.", "KC..Default.", "KC..Default." ,"KC..Default." ,"KC..Default."),
            c("Anon.Student.Id","KC..Default.", "KC..Default.", "KC..Default." ,"KC..Default." ,"KC..Default."),
            c("Anon.Student.Id","KC..Default.", "KC..Default.", "KC..Default." ,"KC..Default." ,"KC..Default."),
            c("Anon.Student.Id","KC..Default.", "KC..Default.", "KC..Default."),
            c("Anon.Student.Id","KC..Default.", "KC..Default.", "KC..Default."),
            c("Anon.Student.Id","KC..Default.", "KC..Default.", "KC..Default." ,"KC..Default." ,"KC..Default."),
            #    c("Anon.Student.Id","KC..Default.", "KC..Default.", "KC..Default." ,"KC..Default." ,"KC..Default."),
            c("Anon.Student.Id","KC..Default.", "KC..Default.", "KC..Default." ,"KC..Default." ,"KC..Default."),
            c("Anon.Student.Id","KC..Default.", "KC..Default.", "KC..Default." ,"KC..Default." ,"KC..Default."),
            c("Anon.Student.Id","KC..Default.", "KC..Default.", "KC..Default." ,"KC..Default." ,"KC..Default."))
featl<-list(c("intercept","intercept",  "linefail",  "linesuc"),
            c("intercept","intercept",  "linefail$", "linesuc$"),
            c("intercept","intercept",  "linefail$", "linefail$", "linesuc$", "linesuc$"),
            #    c("intercept","intercept",  "linefail$", "linefail$", "linesuc$", "linesuc$"),
            c("intercept","intercept",  "linefail", "linefail$", "linesuc", "linesuc$"),
            c("intercept","intercept",  "linefail$", "linefail", "linesuc$", "linesuc"),
            c("intercept","intercept",  "linefail", "linefail", "linesuc", "linesuc"),
            c("intercept","intercept",  "logfail",  "logsuc"),
            c("intercept","intercept",  "logfail$", "logsuc$"),
            c("intercept","intercept",  "logfail$", "logfail$", "logsuc$", "logsuc$"),
            #    c("intercept","intercept",  "logfail$", "logfail$", "logsuc$", "logsuc$"),
            c("intercept","intercept",  "logfail", "logfail$", "logsuc", "logsuc$"),
            c("intercept","intercept",  "logfail$", "logfail", "logsuc$", "logsuc"),
            c("intercept","intercept",  "logfail", "logfail", "logsuc", "logsuc"))
connl<-list(c("+","+","+","+"),
            c("+","+","+","+"),
            c("+","+","+","+","+","+"),
            #    c("+","+","+","*","+","*"),
            c("+","+","+","*","+","*"),
            c("+","+","+","*","+","*"),
            c("+","+","+","*","+","*"),
            c("+","+","+","+"),
            c("+","+","+","+"),
            c("+","+","+","+","+","+"),
            #    c("+","+","+","*","+","*"),
            c("+","+","+","*","+","*"),
            c("+","+","+","*","+","*"),
            c("+","+","+","*","+","*"))
autol <- list(c(F,F,F,F),
              c(F,F,F,F),
              c(F,F,F,T,F,T),
              #    c(F,F,F,T,F,T),
              c(F,F,F,T,F,T),
              c(F,F,F,T,F,T),
              c(F,F,F,T,F,T),
              c(F,F,F,F),
              c(F,F,F,F),
              c(F,F,F,T,F,T),
              #    c(F,F,F,T,F,T),
              c(F,F,F,T,F,T),
              c(F,F,F,T,F,T),
              c(F,F,F,T,F,T))


for(i in 1:length(compl)){
  modelob <<- LKT(data = val_1,components = compl[[i]],features = featl[[i]],connectors = connl[[i]],autoKC = autol[[i]],
                  cv=TRUE,verbose = FALSE,
                  autocent=4)
  cat(mnames[i]," R2cv =  ",mean(modelob$cv_res$mcfad))
  cat(" R2 =  ",modelob$r2,"\n")

  r2s$r2s[i]<-modelob$r2
  r2s$cvr2s[i]<-mean(modelob$cv_res$mcfad)
}

# Additive Factors Model (AFM) fixed effect version
modelob <- LKT(
  data = val_00, interc=TRUE,
  components = c("Anon.Student.Id","KC..Default.","KC..Default."),
  features = c("intercept", "intercept", "lineafm$"))

# Performance Factors Analysis (PFA) fixed effect version
modelob <- LKT(
  data = val_00, interc=TRUE,
  components = c("Anon.Student.Id", "KC..Default.", "KC..Default.", "KC..Default."),
  features = c("intercept", "intercept", "linesuc$","linefail$"))

# have to have prior predictions in data to do the next model in and adaptive system
#   this needs to be added to the data with a first moodel like this
val_00$pred<-modelob$prediction

# PFA using difficulty sensitive predictors (composite model requiring pred from prior model)
modelob <- LKT(
  data = val_00, interc=TRUE,
  components = c("Anon.Student.Id", "KC..Default.", "KC..Default.", "KC..Default."),
  features = c("intercept", "intercept", "linesuc$","linefail$"))
# have to have prior predictions in data to do the next model in and adaptive system
#   this needs to be added to the data wth a first moodel like this
val_00$pred<-modelob$prediction

modelob <- LKT(
  data = val_00, interc=TRUE,
  components = c("Anon.Student.Id", "KC..Default.", "KC..Default.", "KC..Default."),
  features = c("intercept", "intercept", "diffcorComp","linefail"))

# Recent Performance Factors Analysis (RPFA)
modelob <- LKT(
  data = val_00, interc=TRUE,
  components = c("Anon.Student.Id", "KC..Default.", "KC..Default.", "KC..Default."),
  features = c("intercept", "intercept", "propdec2","linefail"),
  fixedpars=c(.9))

# Recency tracing with logitdec
modelob <- LKT(
  data = val_00, interc=TRUE,
  components = c("Anon.Student.Id", "KC..Default.", "KC..Default.", "KC..Default."),
  features = c("intercept", "intercept", "logitdec","recency"),
  fixedpars=c(.9,.5))

# Recency tracing with logitdec and transfer from cluster
modelob <- LKT(
  data = val_00, interc=TRUE,
  components = c("Anon.Student.Id", "KC..Default.", "KC..Default.", "KC..Default.","KC..Cluster."),
  features = c("intercept", "intercept", "logitdec","recency","logitdec"),
  fixedpars=c(.9,.5,.5))


##########################################################################

val_2<-read.csv2("2012-2013-data-with-predictions-4-final.csv")

val_3<-read.table("ds2032_tx_All_Data_114_2017_0207_223411.txt",sep="\t",
                  header=TRUE,na.strings="NA",quote="",comment.char = "")

val_4<-read.table("ds4352_tx_All_Data_6426_2020_0618_064024.txt",sep="\t",
                  header=TRUE,na.strings="NA",quote="",comment.char = "")
