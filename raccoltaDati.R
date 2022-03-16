library(tidyverse)
library(ggjoy)
library(knitr)
library(genius)
library(spotifyr)
library(lubridate)
Sys.setenv(SPOTIFY_CLIENT_ID = 'INSERT_HERE_THE_ID')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'INSERT_HERE_THE_SECRET')
access_token<-get_spotify_access_token()



#dati sporchi

bbbb = get_my_saved_tracks(limit=50, offset=0, authorization = get_spotify_authorization_code(), include_meta_info = FALSE) %>%
  mutate(artists = map_chr(track.artists, function(x) x$name[1]))

for (i in seq(from = 50, to = 1350, by = 50)) {
  bbbb = rbind(bbbb, get_my_saved_tracks(limit=50, offset=i, authorization = get_spotify_authorization_code(), include_meta_info = FALSE) %>%
                 mutate(artists = map_chr(track.artists, function(x) x$name[1])))
  
}

# bbbb = rbind(get_my_saved_tracks(limit=50, offset=0, authorization = get_spotify_authorization_code(), include_meta_info = FALSE) %>%
#             mutate(artists = map_chr(track.artists, function(x) x$name[1])),
#           get_my_saved_tracks(limit=50, offset=50, authorization = get_spotify_authorization_code(), include_meta_info = FALSE) %>%
#             mutate(artists = map_chr(track.artists, function(x) x$name[1])),
#           get_my_saved_tracks(limit=50, offset=100, authorization = get_spotify_authorization_code(), include_meta_info = FALSE) %>%
#             mutate(artists = map_chr(track.artists, function(x) x$name[1])),
#           get_my_saved_tracks(limit=50, offset=150, authorization = get_spotify_authorization_code(), include_meta_info = FALSE) %>%
#             mutate(artists = map_chr(track.artists, function(x) x$name[1])),
#           get_my_saved_tracks(limit=50, offset=200, authorization = get_spotify_authorization_code(), include_meta_info = FALSE) %>%
#             mutate(artists = map_chr(track.artists, function(x) x$name[1])),
#           get_my_saved_tracks(limit=50, offset=250, authorization = get_spotify_authorization_code(), include_meta_info = FALSE) %>%
#             mutate(artists = map_chr(track.artists, function(x) x$name[1])), 
#           get_my_saved_tracks(limit=50, offset=300, authorization = get_spotify_authorization_code(), include_meta_info = FALSE) %>%
#             mutate(artists = map_chr(track.artists, function(x) x$name[1])),
#           get_my_saved_tracks(limit=50, offset=350, authorization = get_spotify_authorization_code(), include_meta_info = FALSE) %>%
#             mutate(artists = map_chr(track.artists, function(x) x$name[1])),
#           get_my_saved_tracks(limit=50, offset=400, authorization = get_spotify_authorization_code(), include_meta_info = FALSE) %>%
#             mutate(artists = map_chr(track.artists, function(x) x$name[1])),
#           get_my_saved_tracks(limit=50, offset=450, authorization = get_spotify_authorization_code(), include_meta_info = FALSE) %>%
#             mutate(artists = map_chr(track.artists, function(x) x$name[1])),
#           get_my_saved_tracks(limit=50, offset=500, authorization = get_spotify_authorization_code(), include_meta_info = FALSE) %>%
#             mutate(artists = map_chr(track.artists, function(x) x$name[1])),
#           get_my_saved_tracks(limit=50, offset=550, authorization = get_spotify_authorization_code(), include_meta_info = FALSE) %>%
#             mutate(artists = map_chr(track.artists, function(x) x$name[1])),
#           get_my_saved_tracks(limit=50, offset=600, authorization = get_spotify_authorization_code(), include_meta_info = FALSE) %>%
#             mutate(artists = map_chr(track.artists, function(x) x$name[1])),
#           get_my_saved_tracks(limit=50, offset=650, authorization = get_spotify_authorization_code(), include_meta_info = FALSE) %>%
#             mutate(artists = map_chr(track.artists, function(x) x$name[1])),
#           get_my_saved_tracks(limit=50, offset=700, authorization = get_spotify_authorization_code(), include_meta_info = FALSE) %>%
#             mutate(artists = map_chr(track.artists, function(x) x$name[1])),
#           get_my_saved_tracks(limit=50, offset=750, authorization = get_spotify_authorization_code(), include_meta_info = FALSE) %>%
#             mutate(artists = map_chr(track.artists, function(x) x$name[1])), 
#           get_my_saved_tracks(limit=50, offset=800, authorization = get_spotify_authorization_code(), include_meta_info = FALSE) %>%
#             mutate(artists = map_chr(track.artists, function(x) x$name[1])),
#           get_my_saved_tracks(limit=50, offset=850, authorization = get_spotify_authorization_code(), include_meta_info = FALSE) %>%
#             mutate(artists = map_chr(track.artists, function(x) x$name[1])),
#           get_my_saved_tracks(limit=50, offset=900, authorization = get_spotify_authorization_code(), include_meta_info = FALSE) %>%
#             mutate(artists = map_chr(track.artists, function(x) x$name[1])),
#           get_my_saved_tracks(limit=50, offset=950, authorization = get_spotify_authorization_code(), include_meta_info = FALSE) %>%
#             mutate(artists = map_chr(track.artists, function(x) x$name[1])),
#           get_my_saved_tracks(limit=50, offset=1000, authorization = get_spotify_authorization_code(), include_meta_info = FALSE) %>%
#             mutate(artists = map_chr(track.artists, function(x) x$name[1])),
#           get_my_saved_tracks(limit=50, offset=1050, authorization = get_spotify_authorization_code(), include_meta_info = FALSE) %>%
#             mutate(artists = map_chr(track.artists, function(x) x$name[1])),
#           get_my_saved_tracks(limit=50, offset=1100, authorization = get_spotify_authorization_code(), include_meta_info = FALSE) %>%
#             mutate(artists = map_chr(track.artists, function(x) x$name[1])),
#           get_my_saved_tracks(limit=50, offset=1150, authorization = get_spotify_authorization_code(), include_meta_info = FALSE) %>%
#             mutate(artists = map_chr(track.artists, function(x) x$name[1])),
#           get_my_saved_tracks(limit=50, offset=1200, authorization = get_spotify_authorization_code(), include_meta_info = FALSE) %>%
#             mutate(artists = map_chr(track.artists, function(x) x$name[1])),
#           get_my_saved_tracks(limit=50, offset=1250, authorization = get_spotify_authorization_code(), include_meta_info = FALSE) %>%
#             mutate(artists = map_chr(track.artists, function(x) x$name[1])), 
#           get_my_saved_tracks(limit=50, offset=1300, authorization = get_spotify_authorization_code(), include_meta_info = FALSE) %>%
#             mutate(artists = map_chr(track.artists, function(x) x$name[1])),
#           get_my_saved_tracks(limit=50, offset=1350, authorization = get_spotify_authorization_code(), include_meta_info = FALSE) %>%
#             mutate(artists = map_chr(track.artists, function(x) x$name[1])))

#dovete mettere il giusto numero di offset che corrisponda al numero dei vostri brani salvati, altrimenti da errore
#(nel mio caso avevo 272 brani salvati quindi mi sono fermato a 250)




#ricavo id, name, traccia, popolarity dai dati sporchi
id=c()
name=c()
track=c()
popolarity=c()

for(i in 1:nrow(bbbb)){
  id=c(id,bbbb[[2]][[i]][2])
  name=c(name, bbbb[[2]][[i]][3])
  track=c(track, as.character(bbbb[i,10]))
  popolarity=c(popolarity, bbbb[i,11])
}

#divido i featuring
name1=c()
name2=c()
name3=c()
for(i in 1:nrow(bbbb)){
  name1=c(name1, name[[i]][1])
  name2=c(name2, name[[i]][2])
  name3=c(name3, name[[i]][3])
}

#prendo solo il primo id nei casi di featuring
id1=c()
for(i in 1:nrow(bbbb)){
  id1=c(id1, id[[i]][1])
}
id1=as.data.frame(id1)

#prendo i generi per ciascun id(artista) (ci mette qualche minuto a girare)
genere1=c()
genere2=c()
genere3=c()
genere4=c()
for(i in 1:nrow(id1)){
  genere1=c(genere1, as.character(get_artist(id=as.character((id1)[i,]), authorization = get_spotify_access_token())[[3]][1]))
  genere2=c(genere2, as.character(get_artist(id=as.character((id1)[i,]), authorization = get_spotify_access_token())[[3]][2]))
  genere3=c(genere3, as.character(get_artist(id=as.character((id1)[i,]), authorization = get_spotify_access_token())[[3]][3]))
  genere4=c(genere4, as.character(get_artist(id=as.character((id1)[i,]), authorization = get_spotify_access_token())[[3]][4]))
  if(i %in% seq(0, 1350, by = 10)){
    print(i)
    Sys.sleep(1)
  }
  
}
genere1=as.data.frame(genere1)
genere2=as.data.frame(genere2)
genere3=as.data.frame(genere3)
genere4=as.data.frame(genere4)


#salvo tutto in dati 
name1=as.data.frame(name1)
name2=as.data.frame(name2)
name3=as.data.frame(name3)
track=as.data.frame(track)
popolarity=as.data.frame(popolarity)

dati=cbind(id1, name1, name2, name3, track, popolarity, genere1, genere2, genere3, genere4)
# dati=cbind(id1, name1, name2, name3, track, popolarity)


#tabelle finali

artisti <- as.data.frame(table(c(as.vector(dati$name1), as.vector(dati$name2), as.vector(dati$name3))))

generi <- as.data.frame(table(c(as.vector(dati$genere1), as.vector(dati$genere2), as.vector(dati$genere3), as.vector(dati$genere4))))

View(artisti)
View(generi)
