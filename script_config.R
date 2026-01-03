install.packages('rsconnect')
install.packages("rsconnect", dependencies = TRUE)
library(rsconnect)

# Configurer le compte

rsconnect::setAccountInfo(name='marcialrmt0108fo',
                          token='1F916B69BD75E828355BE493137259C5',
                          secret='OSIFwe8kr8Kz0pTlyeGQMVOqJVafvtlN3edK+Mys')

#name: = marcialrmt0108fo
# Méthode 2 : Par commande
library(packrat)
library(PKI)
library(rstudioapi)
library(snowflakeauth)
library(jose)
library(RcppTOML)
library(rsconnect)
rsconnect::deployApp(
  appDir = "App_suivi_LabMap_indicator",
  appName = 'Labmap_app',
  account = 'marcialrmt0108fo'
)


####Instal package
install.packages("packrat","PKI","rstudioapi","snowflakeauth", dependencies = TRUE)
