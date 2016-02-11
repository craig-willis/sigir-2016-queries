setwd("/Users/cwillis/dev/uiucGSLIS/sigir-2016-queries/plots")

library(ggplot2)
pdf("topic-groups-ent.pdf");
ent <- read.csv("topic-groups-ent.dat", header=T)
ggplot(ent, aes(factor(Topic), Pct, fill = Code)) + 
  geom_bar(stat="identity", position = "dodge") + 
  labs(x="Topics", x="Percent", title="Entities") +
  scale_fill_grey(start = 0, end = .9) +
  theme_bw(base_size=20) +
  ylim(0, 0.6)
dev.off()

pdf("topic-groups-evt.pdf");
evt <- read.csv("topic-groups-evt.dat", header=T)
ggplot(evt, aes(factor(Topic), Pct, fill = Code)) + 
  geom_bar(stat="identity", position = "dodge") + 
  labs(x="Topics", x="Percent", title="Events") +
  scale_fill_grey(start = 0, end = .9) +
  theme_bw(base_size=20) +
  ylim(0, 0.6)
dev.off()


ag <- read.csv("coder-agreement.dat", header=T)
gp <- ggplot(ag, aes(reorder(Code, -Agreement), Agreement)) + 
  geom_bar(width=0.2, stat = "identity", position = position_dodge(width=1)) + 
  coord_flip() + 
  theme_set(theme_gray(base_size = 10)) + 
  labs(y="Percent agreement", x="Codes")  + ylim(0, 1) 
ggsave(gp, file="coder-agreement.pdf", width=6, height=2)

