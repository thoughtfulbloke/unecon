unec <- read.csv("uneconomicdata.csv")
fullunec <- unec[complete.cases(unec),]
GDPImprovement <- (fullunec$year6 + fullunec$year5 + fullunec$year4)/3 - (fullunec$year3 + fullunec$year2 + fullunec$year1)/3
df <- data.frame(fullunec$Elected,GDPImprovement)
names(df) <- c("OnUNSCinYear4","GDPChange")

d <- density(df$GDPChange[df$OnUNSCinYear4=="n"], na.rm = TRUE)
plot(d, main="Distribution of differences between\nLast and First years", xlab="Difference", ylim=c(0,14))
polygon(d, col="#FF0000CC", border="red")
d <- density(df$GDPChange[df$OnUNSCinYear4=="y"], na.rm = TRUE)
polygon(d, col="#0000FFAA", border="blue")
labels <- c('Not Elected', 'Elected', "Combined Zone")
position <- 'right'
colors <- c("#FF0000CC","#0000FFAA","#8800FFBB")
inset <- c(0.02, 0)
legend(position, labels, fill=colors, inset=inset)
abline(v=0)
text(0.13,14, labels="line of no benefit", cex=0.7)
text(0.3,11, labels="years 4-6 better", cex=0.9, col="#00000099")
text(-0.3,11, labels="years 4-6 worse", cex=0.9, col="#00000099")

