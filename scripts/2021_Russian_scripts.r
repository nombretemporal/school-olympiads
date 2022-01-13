#to qualify for the finals in Russian in 2021: Grade 9: 61.5; Grade 10: 65; Grade 11: 61; source: https://olimpiada.ru/vos2021/russ
all <- read.table("2021_Russian_data.csv", h=TRUE, stringsAsFactors=TRUE, sep=",", quote="")#whole dataset
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
finalists <- subset(all, all$FINAL_PERCENTAGE>=0)#participants of the federal competition
finalists_moscow <- subset(all, all$FINAL_PERCENTAGE>=0 & all$REGION=="Москва")
finalists_moscow_region <- subset(all, all$FINAL_PERCENTAGE>=0 & all$REGION=="Московская область")
finalists_spb <- subset(all, all$FINAL_PERCENTAGE>=0 & all$REGION=="Санкт-Петербург")

#plotting success at the finals against success at the regional level
cairo_pdf("2021_Russian_finals_compared_to_regional.pdf", height=25, width=25)
par(mar=c(5,5,1,1)+0.1, cex.axis=1.5, cex.lab=1.5)
plot(finalists$FINAL_PERCENTAGE ~ finalists$PERCENTAGE, pch=as.character(finalists$REGION), xlab="Процент выполнения на региональном этапе", ylab="Процент выполнения на заключительном этапе", col="white")
abline(lm(finalists$FINAL_PERCENTAGE ~ finalists$PERCENTAGE), lty=3, col="black")
abline(lm(finalists_moscow$FINAL_PERCENTAGE ~ finalists_moscow$PERCENTAGE), lty=3, col="olivedrab")
abline(lm(finalists_moscow_region$FINAL_PERCENTAGE ~ finalists_moscow_region$PERCENTAGE), lty=3, col="tomato")
abline(lm(finalists_spb$FINAL_PERCENTAGE ~ finalists_spb$PERCENTAGE), lty=3, col="cyan")
text(finalists$PERCENTAGE, finalists$FINAL_PERCENTAGE, label=as.character(finalists$REGION), col=colors()[as.numeric(finalists$REGION) + 8])
legend("bottomright",
       c("общая регрессия", "регрессия для Москвы", "регрессия для Московской области", "регрессия для Санкт-Петербурга"),
       col = c("black", "olivedrab", "tomato", "cyan"),
       border = "black",
       lty=3, lwd=5,
       pch="",
       bty = "o",
       box.lwd = par("lwd"),
       box.lty = par("lty"),
       box.col = par("fg"),
       cex = 1.5,
       horiz = FALSE,
       title = NULL
)
dev.off()

#plotting statuses at the finals by region
png("2021_Russian_status_at_finals_by_region.png", h=1000, w=3000)
par(mar=c(20,9,3,1)+0.1, las=2, cex.axis=1.5, cex.lab=1.5, cex.main=1.5)
plot(finalists$FINAL_STATUS ~ finalists$REGION, xlab="", ylab="", main="Статус участников на заключительном этапе по регионам")
dev.off()

#plotting all regional results, one plot for each grade
png("2021_Russian_results_by_region_in_9.png", h=1080, w=1920)
par(mar=c(20,6,1,1)+0.1, cex.axis=1.5, cex.lab=1.5)
plot(first9$PERCENTAGE ~ as.numeric(first9$REGION), axes=FALSE, xlab="", ylab="Процент выполнения победителями, призёрами и участниками (9 класс)", col=rgb(0,1,1,.25), pch=20, ylim=c(0,max(first9$PERCENTAGE)))
points(second9$PERCENTAGE ~ as.numeric(second9$REGION), col=rgb(1,0,1,.25), pch=20)
points(rest9$PERCENTAGE ~ as.numeric(rest9$REGION), col=rgb(0,0,0,.25), pch=20)
axis(2)
axis(1, las=2, at=1:length(levels(real9$REGION)), labels=levels(real9$REGION))
abline(h=61.5, col="green", lty=3)
text(x=7, y=62.5, label="проходной балл заключительного этапа", col="green")
legend("topright",
       c("победители", "призёры", "прочие участники"),
       col = c("cyan", "magenta", "black"),
       border = "black",
       lty=2, lwd=2,
       pch=20,
       bty = "n",
       box.lwd = par("lwd"),
       box.lty = par("lty"),
       box.col = par("fg"),
       cex = 1.5,
       horiz = FALSE,
       title = NULL
)
dev.off()

png("2021_Russian_results_by_region_in_10.png", h=1080, w=1920)
par(mar=c(20,6,1,1)+0.1, cex.axis=1.5, cex.lab=1.5)
plot(first10$PERCENTAGE ~ as.numeric(first10$REGION), axes=FALSE, xlab="", ylab="Процент выполнения победителями, призёрами и участниками (10 класс)", col=rgb(0,1,1,.25), pch=20, ylim=c(0,max(first10$PERCENTAGE)))
points(second10$PERCENTAGE ~ as.numeric(second10$REGION), col=rgb(1,0,1,.25), pch=20)
points(rest10$PERCENTAGE ~ as.numeric(rest10$REGION), col=rgb(0,0,0,.25), pch=20)
axis(2)
axis(1, las=2, at=1:length(levels(real10$REGION)), labels=levels(real10$REGION))
abline(h=65, col="blue", lty=3)
text(x=7, y=66, label="проходной балл заключительного этапа", col="blue")
legend("topright",
       c("победители", "призёры", "прочие участники"),
       col = c("cyan", "magenta", "black"),
       border = "black",
       lty=2, lwd=2,
       pch=20,
       bty = "n",
       box.lwd = par("lwd"),
       box.lty = par("lty"),
       box.col = par("fg"),
       cex = 1.5,
       horiz = FALSE,
       title = NULL
)
dev.off()

png("2021_Russian_results_by_region_in_11.png", h=1080, w=1920)
par(mar=c(20,6,1,1)+0.1, cex.axis=1.5, cex.lab=1.5)
plot(first11$PERCENTAGE ~ as.numeric(first11$REGION), axes=FALSE, xlab="", ylab="Процент выполнения победителями, призёрами и участниками (11 класс)", col=rgb(0,1,1,.25), pch=20, ylim=c(0,max(first11$PERCENTAGE)))
points(second11$PERCENTAGE ~ as.numeric(second11$REGION), col=rgb(1,0,1,.25), pch=20)
points(rest11$PERCENTAGE ~ as.numeric(rest11$REGION), col=rgb(0,0,0,.25), pch=20)
axis(2)
axis(1, las=2, at=1:length(levels(real11$REGION)), labels=levels(real11$REGION))
abline(h=61, col="red", lty=3)
text(x=7, y=62, label="проходной балл заключительного этапа", col="red")
legend("topright",
       c("победители", "призёры", "прочие участники"),
       col = c("cyan", "magenta", "black"),
       border = "black",
       lty=2, lwd=2,
       pch=20,
       bty = "n",
       box.lwd = par("lwd"),
       box.lty = par("lty"),
       box.col = par("fg"),
       cex = 1.5,
       horiz = FALSE,
       title = NULL
)
dev.off()

#plotting regional results with boxplots, one plot for each grade
cairo_pdf("2021_Russian_results_by_region_in_9_with_boxplots.pdf", height=25, width=25)
par(mar=c(20,6,1,1)+0.1, cex.axis=1.5, cex.lab=1.5)
plot(first9$PERCENTAGE ~ as.numeric(first9$REGION), axes=FALSE, xlab="", ylab="Процент выполнения победителями, призёрами и участниками (9 класс)", col=rgb(0,1,1,.25), pch=20, ylim=c(0,max(first9$PERCENTAGE)))
points(second9$PERCENTAGE ~ as.numeric(second9$REGION), col=rgb(1,0,1,.25), pch=20)
points(rest9$PERCENTAGE ~ as.numeric(rest9$REGION), col=rgb(0,0,0,.25), pch=20)
axis(2)
axis(1, las=2, at=1:length(levels(real9$REGION)), labels=levels(real9$REGION))
abline(h=61.5, col="green", lty=3)
text(x=7, y=62.5, label="проходной балл заключительного этапа", col="green")
boxplot(first9$PERCENTAGE ~ first9$REGION, axes=FALSE, xlab="", col=rgb(0,1,1,.03), border=rgb(0,1,1,.3), add=TRUE)
boxplot(second9$PERCENTAGE ~ second9$REGION, axes=FALSE, xlab="", col=rgb(1,0,1,.03), border=rgb(1,0,1,.3), add=TRUE)
legend("topright",
       c("победители", "призёры", "прочие участники"),
       col = c("cyan", "magenta", "black"),
       border = "black",
       lty=2, lwd=2,
       pch=20,
       bty = "n",
       box.lwd = par("lwd"),
       box.lty = par("lty"),
       box.col = par("fg"),
       cex = 1.5,
       horiz = FALSE,
       title = NULL
)
dev.off()

cairo_pdf("2021_Russian_results_by_region_in_10_with_boxplots.pdf", height=25, width=25)
par(mar=c(20,6,1,1)+0.1, cex.axis=1.5, cex.lab=1.5)
plot(first10$PERCENTAGE ~ as.numeric(first10$REGION), axes=FALSE, xlab="", ylab="Процент выполнения победителями, призёрами и участниками (10 класс)", col=rgb(0,1,1,.25), pch=20, ylim=c(0,max(first10$PERCENTAGE)))
points(second10$PERCENTAGE ~ as.numeric(second10$REGION), col=rgb(1,0,1,.25), pch=20)
points(rest10$PERCENTAGE ~ as.numeric(rest10$REGION), col=rgb(0,0,0,.25), pch=20)
axis(2)
axis(1, las=2, at=1:length(levels(real10$REGION)), labels=levels(real10$REGION))
abline(h=65, col="blue", lty=3)
text(x=7, y=66, label="проходной балл заключительного этапа", col="blue")
boxplot(first10$PERCENTAGE ~ first10$REGION, axes=FALSE, xlab="", col=rgb(0,1,1,.03), border=rgb(0,1,1,.3), add=TRUE)
boxplot(second10$PERCENTAGE ~ second10$REGION, axes=FALSE, xlab="", col=rgb(1,0,1,.03), border=rgb(1,0,1,.3), add=TRUE)
legend("topright",
       c("победители", "призёры", "прочие участники"),
       col = c("cyan", "magenta", "black"),
       border = "black",
       lty=2, lwd=2,
       pch=20,
       bty = "n",
       box.lwd = par("lwd"),
       box.lty = par("lty"),
       box.col = par("fg"),
       cex = 1.5,
       horiz = FALSE,
       title = NULL
)
dev.off()

cairo_pdf("2021_Russian_results_by_region_in_11_with_boxplots.pdf", height=25, width=25)
par(mar=c(20,6,1,1)+0.1, cex.axis=1.5, cex.lab=1.5)
plot(first11$PERCENTAGE ~ as.numeric(first11$REGION), axes=FALSE, xlab="", ylab="Процент выполнения победителями, призёрами и участниками (11 класс)", col=rgb(0,1,1,.25), pch=20, ylim=c(0,max(first11$PERCENTAGE)))
points(second11$PERCENTAGE ~ as.numeric(second11$REGION), col=rgb(1,0,1,.25), pch=20)
points(rest11$PERCENTAGE ~ as.numeric(rest11$REGION), col=rgb(0,0,0,.25), pch=20)
axis(2)
axis(1, las=2, at=1:length(levels(real11$REGION)), labels=levels(real11$REGION))
abline(h=61, col="red", lty=3)
text(x=7, y=62, label="проходной балл заключительного этапа", col="red")
boxplot(first11$PERCENTAGE ~ first11$REGION, axes=FALSE, xlab="", col=rgb(0,1,1,.03), border=rgb(0,1,1,.3), add=TRUE)
boxplot(second11$PERCENTAGE ~ second11$REGION, axes=FALSE, xlab="", col=rgb(1,0,1,.03), border=rgb(1,0,1,.3), add=TRUE)
legend("topright",
       c("победители", "призёры", "прочие участники"),
       col = c("cyan", "magenta", "black"),
       border = "black",
       lty=2, lwd=2,
       pch=20,
       bty = "n",
       box.lwd = par("lwd"),
       box.lty = par("lty"),
       box.col = par("fg"),
       cex = 1.5,
       horiz = FALSE,
       title = NULL
)
dev.off()

#plotting mean percentages at the finals as a function of the size of the team
list_of_means <- as.list(NULL)
list_of_team_sizes <- as.list(NULL)
for(i in 1:length(levels(real$REGION))){
	local <- subset(real, as.numeric(real$REGION)==i & real$FINAL_PERCENTAGE>0)
	list_of_means[i] <- mean(local$FINAL_PERCENTAGE)
	list_of_team_sizes[i] <- nrow(local)
}

cairo_pdf("2021_Russian_mean_percentage_at_finals_by_team_size.pdf", height=15, width=15)
par(mar=c(5,5,5,1)+0.1, las=1, cex.axis=1.5, cex.lab=1.5, cex.main=1.5)
plot(unlist(list_of_means) ~ unlist(list_of_team_sizes), log='x', xlab="Количество участников в команде", ylab="Средний процент выполнения участниками команды", main="Средний процент выполнения на заключительном этапе\nв зависимости от числа участников в команде (логарифмическая шкала)", pch="")
text(unlist(list_of_team_sizes), unlist(list_of_means), label=as.character(levels(real$REGION)), col=colors())
dev.off()

#plotting mean percentages at the finals as a function of the mean percentage of prize winners from the respective region at the regional level
list_of_means <- as.list(NULL)
list_of_regional_winner_means <- as.list(NULL)
for(i in 1:length(levels(real$REGION))){
	local <- subset(real, as.numeric(real$REGION)==i & real$FINAL_PERCENTAGE>0)
	localwinners <- subset(real, as.numeric(real$REGION)==i & (real$REGIONAL_STATUS=="Победитель" | real$REGIONAL_STATUS=="Призер"))
	list_of_means[i] <- mean(local$FINAL_PERCENTAGE)
	list_of_regional_winner_means[i] <- mean(localwinners$PERCENTAGE)
}

cairo_pdf("2021_Russian_mean_percentage_at_finals_compared_to_regional_percentages.pdf", height=15, width=15)
par(mar=c(5,5,5,1)+0.1, las=1, cex.axis=1.5, cex.lab=1.5, cex.main=1.5)
plot(unlist(list_of_means) ~ unlist(list_of_regional_winner_means), xlab="Средний процент выполнения победителями и призёрами на региональном этапе", ylab="Средний процент выполнения участниками команды", main="Средний процент выполнения на заключительном этапе\nв зависимости от среднего процента выполнения победителями и призёрами регионального этапа", pch="")
text(unlist(list_of_regional_winner_means), unlist(list_of_means), label=as.character(levels(real$REGION)), col=colors())
abline(lm(unlist(list_of_means) ~ unlist(list_of_regional_winner_means)), lty=2)
dev.off()
