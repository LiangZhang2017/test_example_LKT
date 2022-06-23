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

########################################## Example from Assistments ########################################################
val_1<-read.csv("as.csv") # Assistments
val_1<-val_1[1:50000,]  #Use the first 50000 rows of dataset

val_1$KC..Default.<-val_1$problem_id
val_1$Anon.Student.Id<-val_1$user_id

# make it a datatable
val_1= setDT(val_1)
#make unstratified folds for crossvaldiations
val_1$fold<-sample(1:5,length(val_1$Anon.Student.Id),replace=T)

#val_1$CF..Time.<-as.numeric(as.POSIXct(as.character(val_1$Time),format="%Y-%m-%d %H:%M:%S"))

# get the times of each trial in seconds from 1970
#val$CF..Time.<-as.numeric(as.POSIXct(as.character(val$Time),format="%Y-%m-%d %H:%M:%S"))

#make sure it is ordered in the way the code expects
val_1<-val_1[order(val_1$Anon.Student.Id),]
val_1$CF..ansbin<-as.numeric(val_1$correct)

# create durations
val_1$Duration..sec.<-as.numeric(val_1$ms_first_response)/1000

# this function needs times and durations but you don't need it if you don't want to model time effects
val_1 <- computeSpacingPredictors(val_1, "KC..Default.") #allows recency, spacing, forgetting features to run

val_11<-setDT(read.csv("as.csv"))
val_11<-val_11[1:50000,]
val_11$Anon.Student.Id<-as.numeric(val_11$user_id)
val_11$CF..ansbin.<-as.numeric(val_11$correct)
val_11$Outcome<-ifelse((val_11$correct)==1,"CORRECT","INCORRECT")
val_11$KC..Default.<-val_11$skill_name

val_11$CF..Time.<-as.numeric(val_11$order_id)
val_11$Duration..sec.<-as.numeric(val_1$ms_first_response)/1000


val_11$fold<-sample(1:5,length(val_11$Anon.Student.Id),replace = T)
val_11<-val_11[order(val_11$Anon.Student.Id),]

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
  modelob <<- LKT(data = val_11,components = compl[[i]],features = featl[[i]],connectors = connl[[i]],autoKC = autol[[i]],
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
  data = val_11, interc=TRUE,
  components = c("Anon.Student.Id","KC..Default.","KC..Default."),
  features = c("intercept", "intercept", "lineafm$"))

# Performance Factors Analysis (PFA) fixed effect version

modelob <- LKT(
  data = val_11, interc=TRUE,
  components = c("Anon.Student.Id", "KC..Default.", "KC..Default.", "KC..Default."),
  features = c("intercept", "intercept", "linesuc$","linefail$"))
# have to have prior predictions in data to do the next model in and adaptive system
#   this needs to be added to the data wth a first moodel like this
val_11$pred<-modelob$prediction

# PFA using difficulty sensitive predictors (composite model requiring pred from prior model)

modelob <- LKT(
  data = val_11, interc=TRUE,
  components = c("Anon.Student.Id", "KC..Default.", "KC..Default.", "KC..Default."),
  features = c("intercept", "intercept", "linesuc$","linefail$"))
# have to have prior predictions in data to do the next model in and adaptive system
#   this needs to be added to the data wth a first moodel like this
val_11$pred<-modelob$prediction

modelob <- LKT(
  data = val_11, interc=TRUE,
  components = c("Anon.Student.Id", "KC..Default.", "KC..Default.", "KC..Default."),
  features = c("intercept", "intercept", "diffcorComp","linefail"))

# Recent Performance Factors Analysis (RPFA)
modelob <- LKT(
  data = val_11, interc=TRUE,
  components = c("Anon.Student.Id", "KC..Default.", "KC..Default.", "KC..Default."),
  features = c("intercept", "intercept", "propdec2","linefail"),
  fixedpars=c(.9))

# Recency tracing with logitdec

modelob <- LKT(
  data = val_11, interc=TRUE,
  components = c("Anon.Student.Id", "KC..Default.", "KC..Default.", "KC..Default."),
  features = c("intercept", "intercept", "logitdec","recency"),
  fixedpars=c(.9,.5))



