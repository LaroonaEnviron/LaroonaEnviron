str(Vol)
unique(Vol$caisson)

with(subset(Vol, caisson %in% c("4","4A","4B")), plot(Date,vol, type='l'))

caiss04 <- subset(Vol, caisson %in% c("4","4A"))
caiss04 <- caiss04[order(caiss04$caisson),]
caiss04$vol <- with(caiss04, ifelse(Date < "1999-10-01", vol/1000, vol))
caiss04
caiss04$vol <- ifelse(caiss04$Date >="1999-10-01" & caiss04$Date <= "2003-11-03" & caiss04$caisson=="4", caiss04$vol*1000, caiss04$vol)
caiss04

with(caiss04, plot(Date, vol, type='l'))
N <- nrow(caiss04)
caiss04$pumpd[2:N] <- with(caiss04, vol[2:N] - vol[1:(N-1)])
caiss04$pumpd <- with(caiss04, ifelse(pumpd < 0, NA, pumpd))
caiss04
with(subset(caiss04, Date > "1998-01-01"), {
  plot(Date, pumpd, type='h', las=1)
  })

# repeat for caisson 5
with(subset(Vol, caisson %in% c("5","5_")), plot(Date,vol, type='l'))

caiss05 <- subset(Vol, caisson %in% c("5","5_"))
caiss05 <- caiss05[order(caiss05$caisson),]

caiss05$vol <- with(caiss05, ifelse(Date < "1999-10-01", vol/1000, vol))
caiss05
# caiss05$vol <- with(caiss05, ifelse(Date >="1999-10-01" & Date <= "2003-11-03" & caisson=="5", caiss05$vol*1000, caiss05$vol)
caiss05

with(caiss05, plot(Date, vol, type='l'))
N <- nrow(caiss05)
caiss05$pumpd[2:N] <- with(caiss05, vol[2:N] - vol[1:(N-1)])
caiss05$pumpd <- with(caiss05, ifelse(pumpd < 0, NA, pumpd))
caiss05
with(caiss05, {
  plot(Date, pumpd, type='h', las=1)
  })
