mrc$Texture <- factor(mrc$Texture, levels=c("S","SL","LS","L","SCL","CL","C"), ordered=T)
mrc$Field_Text <- factor(mrc$Field_Text, levels=c("S","SL","LS","SCL","LSCL","CL","C"), ordered=T)
mrc$Field_CO3 <- factor(mrc$Field_CO3, levels=c("N","S","M","H","VH"), ordered=T)
str(mrc)
mrc.srt <- with(mrc, mrc[order(Sand, Field_CO3, raw1, raw2, taw),])
head(mrc.srt,20)
with(mrc.srt, plot(Sand, raw1, pch=15+as.integer(Field_CO3), col=as.integer(Field_Text)))
(taw.max <- max(mrc$taw))
which(mrc.srt$taw==taw.max)

mrc.srt[1:5, c("Sand","taw")]
Sx <- sqrt(var(mrc.srt$Sand))
Sy <- sqrt(var(mrc.srt$taw))
Sx;Sy
(cen <- mrc.srt[1:4,"taw"])
(plus <- which(mrc.srt$taw < max(cen,na.rm=T)))
(minus <- which(mrc.srt$taw < cen-4))
length(unique(c(minus,plus)))
