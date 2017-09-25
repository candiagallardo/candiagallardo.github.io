
### Estimando localizações de maracanãs (CEMAVE - ICMBio, Curaçá, BA)
### Carlos Candia-Gallardo, setembro de 2017

#Carregando os pacotes necessários
library(sigloc) #carregando o pacote sigloc que faz triangulações
library(rgdal) #carregando pacote para trabalhar com dados espaciais
library(maptools) #carregando pacote para trabalhar com dados espaciais e mapas

##Dados maracanã-----
data <- read.csv("dados.csv", sep = ";", dec = ",") # Importando os dados
 
## Convertendo coordenadas dos pontos de medição de lat long para UTM------
    ## Agora vamos converter as coordenadas de lat long para utm
    pontos <- data[,c(3,5:6)] #extrai as colunas de grupo (GID) e de coordenadas
    coordinates(pontos)<- c("lon", "lat") #converte as coordenadas em classe SpatialPoints
    BNG<- CRS("+init=epsg:27700 +proj=longlat +ellps=WGS84 +datum=WGS84 +south") #criando a projeção, datum, etc
    proj4string(pontos)<-BNG #inserindo projeção
    #criando projeção utm
    p4s <- CRS("+init=epsg:27700 +proj=utm +zone=24L +ellps=WGS84 +datum=WGS84 +south") 
    pontos_wgs84 <- spTransform(pontos, CRS= p4s) #transformando as coordenadas em utm
    plot(pontos_wgs84$lon, pontos_wgs84$lat) #plotando os pontos

        #Exportando kml (lat) com os pontos de medição
        writeOGR(pontos, dsn="pontos_medicao.kml", layer= "ponto", driver="KML", dataset_options=c("NameField=name"))
        
## Criando a tabela com os dados e preparando dados------
        df <- data.frame(
          Date = data[,1],
          Observers = data[,2],
          GID = data[,3],
          Time = data[,4],
          Easting = as.data.frame(pontos_wgs84)[,2],
          Northing = as.data.frame(pontos_wgs84)[,3],
          Azimuth = data[,7]
        )   

    #corrigindo a declinação magnética (23° 2' W)
    library(measurements) #pacote para converter graus minutos decimais em graus decimais
        
    declin <- "23°2" ## descobri a declinação de Curaçá (Aracaju) em https://www.ngdc.noaa.gov/geomag-web/
    declin = gsub('°', ' ', declin) #substituindo o símbolo "°" por espaço " "
    declin = as.integer(conv_unit(declin, from = 'deg_dec_min', to = 'dec_deg'))
    data[,7] <- as.integer(round(df[,7] + declin))
    for(i in 1:nrow(df)) {
      if(df$Azimuth[i] > 360){
        df$Azimuth[i] = as.integer(df$Azimuth[i] - 360)
      }
    }

###triangulando-----

  meds <- as.receiver(df)
  plot(meds,bearings=T,xlab="Easting",ylab="Northing",ylim = c(8982000,8987000), xlim = c(427200,479000))
  int <- findintersects(meds)
  plot(int, add = T, col = "blue")
  loc<-locate(meds)
  ## Display results with a different color for bad points
  plot(loc,badcolor=TRUE,errors = TRUE, add=F,col=c("blue","red"))
  
  #mesmo plot cortando o ponto outlier
  plot(loc,badcolor=TRUE,errors = TRUE, add=F,col=c("blue","red"), xlim = c(425000,432000),ylim = c(8975000,8987000), xlab = "Easting", ylab = "Northing")

#Exportando as estimativas de localização em kml (tem que converter em lat long)
  posicoes <- data.frame(loc) #transformando as estimativas de localização em data frame
  posicoes <- posicoes[-which(is.na(posicoes)),] #eliminando NA (posições que não puderam ser estimadas)
  coordinates(posicoes)<- c("X", "Y") #transformando coordenadas em SpatialPoints
  BNG<- CRS("+init=epsg:27700 +proj=utm +zone=24L +ellps=WGS84 +datum=WGS84 +south")#ajustando projeção
  proj4string(posicoes)<-BNG
  p4s <- CRS("+init=epsg:27700 +proj=longlat +ellps=WGS84 +datum=WGS84") #convertendo em lat long
  posicoes<- spTransform(posicoes, CRS= p4s)
  plot(posicoes$X, posicoes$Y) #plotando os pontos
  
      #Exportando kml
      writeOGR(posicoes, dsn="estimativas_posicao_maracanas.kml", layer= "estimativas", driver="KML", dataset_options=c("NameField=name"))
