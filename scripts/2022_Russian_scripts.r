all <- read.table("2022_Russian_data.csv", h=TRUE, stringsAsFactors=TRUE, sep=",", quote="")#whole dataset
real <- subset(all, all$REGIONAL_STATUS=="Победитель" | all$REGIONAL_STATUS=="Призер" | all$REGIONAL_STATUS=="Участник" | all$REGIONAL_STATUS=="")#participated, not disqualified
real9 <- subset(all, (all$REGIONAL_STATUS=="Победитель" | all$REGIONAL_STATUS=="Призер" | all$REGIONAL_STATUS=="Участник" | all$REGIONAL_STATUS=="") & all$GRADE_OF_COMPETITION==9)
real10 <- subset(all, (all$REGIONAL_STATUS=="Победитель" | all$REGIONAL_STATUS=="Призер" | all$REGIONAL_STATUS=="Участник" | all$REGIONAL_STATUS=="") & all$GRADE_OF_COMPETITION==10)
real11 <- subset(all, (all$REGIONAL_STATUS=="Победитель" | all$REGIONAL_STATUS=="Призер" | all$REGIONAL_STATUS=="Участник" | all$REGIONAL_STATUS=="") & all$GRADE_OF_COMPETITION==11)
awards <- subset(all, all$REGIONAL_STATUS=="Победитель" | all$REGIONAL_STATUS=="Призер")#all awarded
first <- subset(all, all$REGIONAL_STATUS=="Победитель")#all first prizes
second <- subset(all, all$REGIONAL_STATUS=="Призер")#all second prizes
awards9 <- subset(all, (all$REGIONAL_STATUS=="Победитель" | all$REGIONAL_STATUS=="Призер") & all$GRADE_OF_COMPETITION==9)
awards10 <- subset(all, (all$REGIONAL_STATUS=="Победитель" | all$REGIONAL_STATUS=="Призер") & all$GRADE_OF_COMPETITION==10)
awards11 <- subset(all, (all$REGIONAL_STATUS=="Победитель" | all$REGIONAL_STATUS=="Призер") & all$GRADE_OF_COMPETITION==11)
first9 <- subset(all, all$REGIONAL_STATUS=="Победитель" & all$GRADE_OF_COMPETITION==9)
first10 <- subset(all, all$REGIONAL_STATUS=="Победитель" & all$GRADE_OF_COMPETITION==10)
first11 <- subset(all, all$REGIONAL_STATUS=="Победитель" & all$GRADE_OF_COMPETITION==11)
second9 <- subset(all, all$REGIONAL_STATUS=="Призер" & all$GRADE_OF_COMPETITION==9)
second10 <- subset(all, all$REGIONAL_STATUS=="Призер" & all$GRADE_OF_COMPETITION==10)
second11 <- subset(all, all$REGIONAL_STATUS=="Призер" & all$GRADE_OF_COMPETITION==11)
rest9 <- subset(all, (all$REGIONAL_STATUS=="Участник" | all$REGIONAL_STATUS=="") & all$GRADE_OF_COMPETITION==9)
rest10 <- subset(all, (all$REGIONAL_STATUS=="Участник" | all$REGIONAL_STATUS=="") & all$GRADE_OF_COMPETITION==10)
rest11 <- subset(all, (all$REGIONAL_STATUS=="Участник" | all$REGIONAL_STATUS=="") & all$GRADE_OF_COMPETITION==11)

#plotting all regional results, one plot for each grade
png("2022_results_by_region_in_9.png", h=1080, w=1920)
par(mar=c(20,6,1,1)+0.1, cex.axis=1.5, cex.lab=1.5)
plot(rest9$PERCENTAGE ~ as.numeric(rest9$REGION), axes=FALSE, xlab="", ylab="Процент выполнения победителями, призёрами и участниками (9 класс)", col=rgb(0,0,0,.25), pch=20, ylim=c(0,max(first9$PERCENTAGE)))
points(second9$PERCENTAGE ~ as.numeric(second9$REGION), col=rgb(1,0,1,.25), pch=20)
points(first9$PERCENTAGE ~ as.numeric(first9$REGION), col=rgb(0,1,1,.25), pch=20)
axis(2)
axis(1, las=2, at=1:length(levels(real9$REGION)), labels=levels(real9$REGION))
legend("topright",
       c("победители", "призёры", "прочие участники"),
       #fill= "blue",
       col = c("cyan", "magenta", "black"),
       border = "black",
       lty=2, lwd=2,
       pch=20,
       bty = "n",
       #bg = par("bg"),
       box.lwd = par("lwd"),
       box.lty = par("lty"),
       box.col = par("fg"),
       cex = 1.5,
       horiz = FALSE,
       title = NULL
)
dev.off()

png("2022_results_by_region_in_10.png", h=1080, w=1920)
par(mar=c(20,6,1,1)+0.1, cex.axis=1.5, cex.lab=1.5)
plot(rest10$PERCENTAGE ~ as.numeric(rest10$REGION), axes=FALSE, xlab="", ylab="Процент выполнения победителями, призёрами и участниками (10 класс)", col=rgb(0,0,0,.25), pch=20, ylim=c(0,max(first10$PERCENTAGE)))
points(second10$PERCENTAGE ~ as.numeric(second10$REGION), col=rgb(1,0,1,.25), pch=20)
points(first10$PERCENTAGE ~ as.numeric(first10$REGION), col=rgb(0,1,1,.25), pch=20)
axis(2)
axis(1, las=2, at=1:length(levels(real10$REGION)), labels=levels(real10$REGION))
legend("topright",
       c("победители", "призёры", "прочие участники"),
       #fill= "blue",
       col = c("cyan", "magenta", "black"),
       border = "black",
       lty=2, lwd=2,
       pch=20,
       bty = "n",
       #bg = par("bg"),
       box.lwd = par("lwd"),
       box.lty = par("lty"),
       box.col = par("fg"),
       cex = 1.5,
       horiz = FALSE,
       title = NULL
)
dev.off()

png("2022_results_by_region_in_11.png", h=1080, w=1920)
par(mar=c(20,6,1,1)+0.1, cex.axis=1.5, cex.lab=1.5)
plot(rest11$PERCENTAGE ~ as.numeric(rest11$REGION), axes=FALSE, xlab="", ylab="Процент выполнения победителями, призёрами и участниками (11 класс)", col=rgb(0,0,0,.25), pch=20, ylim=c(0,max(first11$PERCENTAGE)))
points(second11$PERCENTAGE ~ as.numeric(second11$REGION), col=rgb(1,0,1,.25), pch=20)
points(first11$PERCENTAGE ~ as.numeric(first11$REGION), col=rgb(0,1,1,.25), pch=20)
axis(2)
axis(1, las=2, at=1:length(levels(real11$REGION)), labels=levels(real11$REGION))
legend("topright",
       c("победители", "призёры", "прочие участники"),
       #fill= "blue",
       col = c("cyan", "magenta", "black"),
       border = "black",
       lty=2, lwd=2,
       pch=20,
       bty = "n",
       #bg = par("bg"),
       box.lwd = par("lwd"),
       box.lty = par("lty"),
       box.col = par("fg"),
       cex = 1.5,
       horiz = FALSE,
       title = NULL
)
dev.off()

#plotting regional results with boxplots, one plot for each grade
cairo_pdf("2022_results_by_region_in_9_with_boxplots.pdf", height=25, width=25)
par(mar=c(20,6,1,1)+0.1, cex.axis=1.5, cex.lab=1.5)
plot(rest9$PERCENTAGE ~ as.numeric(rest9$REGION), axes=FALSE, xlab="", ylab="Процент выполнения победителями, призёрами и участниками (9 класс)", col=rgb(0,0,0,.25), pch=20, ylim=c(0,max(first9$PERCENTAGE)))
points(second9$PERCENTAGE ~ as.numeric(second9$REGION), col=rgb(1,0,1,.25), pch=20)
points(first9$PERCENTAGE ~ as.numeric(first9$REGION), col=rgb(0,1,1,.25), pch=20)
axis(2)
axis(1, las=2, at=1:length(levels(real9$REGION)), labels=levels(real9$REGION))
boxplot(first9$PERCENTAGE ~ first9$REGION, axes=FALSE, xlab="", col=rgb(0,1,1,.03), border=rgb(0,1,1,.3), add=TRUE)
boxplot(second9$PERCENTAGE ~ second9$REGION, axes=FALSE, xlab="", col=rgb(1,0,1,.03), border=rgb(1,0,1,.3), add=TRUE)
legend("topright",
       c("победители", "призёры", "прочие участники"),
       #fill= "blue",
       col = c("cyan", "magenta", "black"),
       border = "black",
       lty=2, lwd=2,
       pch=20,
       bty = "n",
       #bg = par("bg"),
       box.lwd = par("lwd"),
       box.lty = par("lty"),
       box.col = par("fg"),
       cex = 1.5,
       horiz = FALSE,
       title = NULL
)
dev.off()

cairo_pdf("2022_results_by_region_in_10_with_boxplots.pdf", height=25, width=25)
par(mar=c(20,6,1,1)+0.1, cex.axis=1.5, cex.lab=1.5)
plot(rest10$PERCENTAGE ~ as.numeric(rest10$REGION), axes=FALSE, xlab="", ylab="Процент выполнения победителями, призёрами и участниками (10 класс)", col=rgb(0,0,0,.25), pch=20, ylim=c(0,max(first10$PERCENTAGE)))
points(second10$PERCENTAGE ~ as.numeric(second10$REGION), col=rgb(1,0,1,.25), pch=20)
points(first10$PERCENTAGE ~ as.numeric(first10$REGION), col=rgb(0,1,1,.25), pch=20)
axis(2)
axis(1, las=2, at=1:length(levels(real10$REGION)), labels=levels(real10$REGION))
boxplot(first10$PERCENTAGE ~ first10$REGION, axes=FALSE, xlab="", col=rgb(0,1,1,.03), border=rgb(0,1,1,.3), add=TRUE)
boxplot(second10$PERCENTAGE ~ second10$REGION, axes=FALSE, xlab="", col=rgb(1,0,1,.03), border=rgb(1,0,1,.3), add=TRUE)
legend("topright",
       c("победители", "призёры", "прочие участники"),
       #fill= "blue",
       col = c("cyan", "magenta", "black"),
       border = "black",
       lty=2, lwd=2,
       pch=20,
       bty = "n",
       #bg = par("bg"),
       box.lwd = par("lwd"),
       box.lty = par("lty"),
       box.col = par("fg"),
       cex = 1.5,
       horiz = FALSE,
       title = NULL
)
dev.off()

cairo_pdf("2022_results_by_region_in_11_with_boxplots.pdf", height=25, width=25)
par(mar=c(20,6,1,1)+0.1, cex.axis=1.5, cex.lab=1.5)
plot(rest11$PERCENTAGE ~ as.numeric(rest11$REGION), axes=FALSE, xlab="", ylab="Процент выполнения победителями, призёрами и участниками (11 класс)", col=rgb(0,0,0,.25), pch=20, ylim=c(0,max(first11$PERCENTAGE)))
points(second11$PERCENTAGE ~ as.numeric(second11$REGION), col=rgb(1,0,1,.25), pch=20)
points(first11$PERCENTAGE ~ as.numeric(first11$REGION), col=rgb(0,1,1,.25), pch=20)
axis(2)
axis(1, las=2, at=1:length(levels(real11$REGION)), labels=levels(real11$REGION))
boxplot(first11$PERCENTAGE ~ first11$REGION, axes=FALSE, xlab="", col=rgb(0,1,1,.03), border=rgb(0,1,1,.3), add=TRUE)
boxplot(second11$PERCENTAGE ~ second11$REGION, axes=FALSE, xlab="", col=rgb(1,0,1,.03), border=rgb(1,0,1,.3), add=TRUE)
legend("topright",
       c("победители", "призёры", "прочие участники"),
       #fill= "blue",
       col = c("cyan", "magenta", "black"),
       border = "black",
       lty=2, lwd=2,
       pch=20,
       bty = "n",
       #bg = par("bg"),
       box.lwd = par("lwd"),
       box.lty = par("lty"),
       box.col = par("fg"),
       cex = 1.5,
       horiz = FALSE,
       title = NULL
)
dev.off()

