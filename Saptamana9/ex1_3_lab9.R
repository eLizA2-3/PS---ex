
table <- read.csv("life_expect.csv", header=TRUE, sep=',')

tara <- table[["country"]]
male <- table[["male"]]
female <- table[["female"]]

cases <- rbind(male, female)

barplot(cases,
        beside=TRUE,
        col=c("green", "skyblue1"),
        names.arg=tara,las=2,
        legend=c("Male", "Female"),
        args.legend=list(x="topright", cex=0.75))
