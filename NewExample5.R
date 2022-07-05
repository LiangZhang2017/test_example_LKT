
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

########################################## Example from Computer Programming ########################################################
val_5<-read.table("ds4584_tx_All_Data_6676_2020_1101_035031.txt",sep="\t",
                  header=TRUE,na.strings="NA",quote="",comment.char = "")
val_5<-setDT(val_5)
# use the default "$Anon.Student.Id","$Duration..sec".
val_5$KC..Default.<-val_5$KC..Concepts.

val_5$fold<-sample(1:5,length(val_5$Anon.Student.Id),replace=T)
# get the times of each trial in seconds from 1970
val_5$CF..Time.<-as.numeric(as.POSIXct(as.character(val_5$Time),format="%Y-%m-%d %H:%M:%S"))
#make sure it is ordered in the way the code expects
val_5<-val_5[order(val_5$Anon.Student.Id, val_5$CF..Time.),]

#create a binary response column to predict and extract only data with a valid value
val_5$CF..ansbin.<-ifelse(tolower(val_5$Outcome)=="correct",1,ifelse(tolower(val_5$Outcome)=="incorrect",0,-1))
val_5<-val_5[val_5$CF..ansbin==0 | val_5$CF..ansbin.==1,]
val_5<- computeSpacingPredictors(val_5, "KC..Default.")

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
  modelob <<- LKT(data = val_5,components = compl[[i]],features = featl[[i]],connectors = connl[[i]],autoKC = autol[[i]],
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





