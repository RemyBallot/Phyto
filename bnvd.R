library(readr)
library(testthat)
library(readxl)
library(stringr)
library(testthat)

load("dpt.rda")

bnvd_2013 <- read.csv("carto_init/bnvdAcheteur/BNVD_INERIS_20180313_PRESSIONS_COMMUNE_PRODUIT_DETAILLE_2013_1267_1389_20171229V5.csv",fileEncoding="CP1252",sep=";",skip = 2)
bnvd_2014 <- read.csv("carto_init/bnvdAcheteur/BNVD_INERIS_20180313_PRESSIONS_COMMUNE_PRODUIT_DETAILLE_2014_1267_1389_20171229V5.csv",fileEncoding="CP1252",sep=";",skip = 2)
bnvd_2015 <- read.csv("carto_init/bnvdAcheteur/BNVD_INERIS_20180313_PRESSIONS_COMMUNE_PRODUIT_DETAILLE_2015_1267_1389_20171229V5.csv",fileEncoding="CP1252",sep=";",skip = 2)
bnvd_2016 <- read.csv("carto_init/bnvdAcheteur/BNVD_INERIS_20180313_PRESSIONS_COMMUNE_PRODUIT_DETAILLE_2016_1267_1389_20171229V5.csv",fileEncoding="CP1252",sep=";",skip = 2)
bnvd_2017 <- read.csv("carto_init/bnvdAcheteur/BNVD_INERIS_20180313_PRESSIONS_COMMUNE_PRODUIT_DETAILLE_2017_1267_1389_20171229V5.csv",fileEncoding="CP1252",sep=";",skip = 2)

BNVD<- rbind(bnvd_2013,bnvd_2014,bnvd_2015,bnvd_2016,bnvd_2017)
expect_equal(nrow(BNVD),(nrow(bnvd_2013)+nrow(bnvd_2014)+nrow(bnvd_2015)+nrow(bnvd_2016)+nrow(bnvd_2017)))

Annee_BNVD<- c(rep("2013",nrow(bnvd_2013)),rep("2014", nrow(bnvd_2014)), rep("2015", nrow(bnvd_2015)),rep("2016", nrow(bnvd_2016)), rep("2017", nrow(bnvd_2017)))
BNVD2 <- cbind(BNVD, Annee_BNVD)
expect_equal(nrow(BNVD2),nrow(BNVD))

## Passer du code postal à depart à reg
vect<- BNVD2$Code.postal.acheteur
remplacer<- function(vect){str_sub(vect,1,-4)}
v<-lapply(vect, remplacer)
BNVD3 <- cbind(BNVD2, CODE_DEPT= unlist(v))
expect_equal(nrow(BNVD3),nrow(BNVD2))

iconv.data.frame<-function(df,...){ 
  df.names<-iconv(names(df),...) 
  df.rownames<-iconv(rownames(df),...) 
  names(df)<-df.names 
  rownames(df)<-df.rownames 
  df.list<-lapply(df,function(x){ 
    if(class(x)=="factor"){x<-factor(iconv(as.character(x),...))}else 
      if(class(x)=="character"){x<-iconv(x,...)}else{x} 
  }) 
  df.new<-do.call("data.frame",df.list) 
  return(df.new) 
} 

BNVD4=iconv.data.frame(BNVD3)
expect_equal(nrow(BNVD4),nrow(BNVD3))

BNVD5<- merge(BNVD4, dpt, by= "CODE_DEPT",all=T)
expect_equal(nrow(BNVD5),nrow(BNVD4))

save(BNVD5,file ="BNVD.rda")

BNVD_2014<-BNVD5[BNVD5$Annee_BNVD == "2014",]
expect_equal(nrow(BNVD_2014),nrow(bnvd_2014))

save(BNVD_2014,file ="BNVD_2014.rda")
