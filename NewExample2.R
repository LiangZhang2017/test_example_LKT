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

########################################## Example from ALEKS ########################################################
val_2<-read.table("ds2032_tx_All_Data_114_2017_0207_223411.txt",sep="\t",
                  header=TRUE,na.strings="NA",quote="",comment.char = "")

val_2<-val_2[1:50000,]  #Use the first 50000 rows of dataset

# use the default "$Anon.Student.Id","$KC..Default","$Duration..sec".
val_2$CF..ansbin.<-ifelse(tolower(val_2$Outcome)=="correct",1,ifelse(tolower(val_2$Outcome)=="incorrect",0,-1))
val_2<-val_2[val_2$CF..ansbin==0 | val_2$CF..ansbin.==1,]

# get the times of each trial in seconds from 1970
val_2$CF..Time.<-as.numeric(as.POSIXct(as.character(val_2$Time),format="%Y-%m-%d %H:%M:%S"))
#make sure it is ordered in the way the code expects
val_2<-val_2[order(val_2$Anon.Student.Id, val_2$CF..Time.),]
val_2 <- computeSpacingPredictors(val_2, "KC..Default.")


val_22<-setDT(read.table("ds2032_tx_All_Data_114_2017_0207_223411.txt",sep="\t",
                         header=TRUE,na.strings="NA",quote="",comment.char = ""))
val_22<-val_22[1:50000,]  #Use the first 50000 rows of dataset

val_22$CF..ansbin.<-ifelse(tolower(val_22$Outcome)=="correct",1,ifelse(tolower(val_22$Outcome)=="incorrect",0,-1))
val_22<-val_22[val_22$CF..ansbin==0 | val_22$CF..ansbin.==1,]
val_22$CF..Time.<-as.numeric(as.POSIXct(as.character(val_22$Time),format="%Y-%m-%d %H:%M:%S"))

#make unstratified folds for crossvaldiations
val_22$fold<-sample(1:5,length(val_22$Anon.Student.Id),replace=T)
#make sure it is ordered in the way the code expects
val_22<-val_22[order(val_22$Anon.Student.Id, val_22$CF..Time.),]

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
  modelob <<- LKT(data = val_22,components = compl[[i]],features = featl[[i]],connectors = connl[[i]],autoKC = autol[[i]],
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
  data = val_22, interc=TRUE,
  components = c("Anon.Student.Id","KC..Default.","KC..Default."),
  features = c("intercept", "intercept", "lineafm$"))

# Performance Factors Analysis (PFA) fixed effect version

modelob <- LKT(
  data = val_22, interc=TRUE,
  components = c("Anon.Student.Id", "KC..Default.", "KC..Default.", "KC..Default."),
  features = c("intercept", "intercept", "linesuc$","linefail$"))
# have to have prior predictions in data to do the next model in and adaptive system
#   this needs to be added to the data wth a first moodel like this
val_22$pred<-modelob$prediction

# PFA using difficulty sensitive predictors (composite model requiring pred from prior model)

modelob <- LKT(
  data = val_22, interc=TRUE,
  components = c("Anon.Student.Id", "KC..Default.", "KC..Default.", "KC..Default."),
  features = c("intercept", "intercept", "linesuc$","linefail$"))
# have to have prior predictions in data to do the next model in and adaptive system
#   this needs to be added to the data wth a first moodel like this
val_22$pred<-modelob$prediction

modelob <- LKT(
  data = val_22, interc=TRUE,
  components = c("Anon.Student.Id", "KC..Default.", "KC..Default.", "KC..Default."),
  features = c("intercept", "intercept", "diffcorComp","linefail"))

# Recent Performance Factors Analysis (RPFA)
modelob <- LKT(
  data = val_22, interc=TRUE,
  components = c("Anon.Student.Id", "KC..Default.", "KC..Default.", "KC..Default."),
  features = c("intercept", "intercept", "propdec2","linefail"),
  fixedpars=c(.9))

# Recency tracing with logitdec

modelob <- LKT(
  data = val_22, interc=TRUE,
  components = c("Anon.Student.Id", "KC..Default.", "KC..Default.", "KC..Default."),
  features = c("intercept", "intercept", "logitdec","recency"),
  fixedpars=c(.9,.5))


