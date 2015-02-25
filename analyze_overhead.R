#! /usr/bin/env Rscript

# in inches
figure.width <- 7
# ratio <- 2/3
ratio <- 4/5
figure.height <- 2



tsv_name.default <- csv.overall <- "output/jitsummaries/jit-overhead.csv"
csv.cross <- "output/jitsummaries/jit-overhead-cross.csv"
csv.shoot <- "output/jitsummaries/jit-overhead-shootout.csv"
csv.chap <- "output/jitsummaries/jit-overhead-chap.csv"


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
if (FALSE) {
  setwd("~/dev/pypy/pycket-bench")
  cmd.line <- FALSE
} else {
  cmd.line <- TRUE
}

pkgs = c(
  "reshape2",
  "plyr",
  #"beanplot",
  "boot",
  "Hmisc",
  "ggplot2",
  "tools",
  "xlsx",
  "dplyr"
)
source("./help.R")
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


overall <- read.csv(csv.overall,  col.names=c('benchid', 'jit', 'total', 'ratio')) 
overall <- overall %>% mutate(variant='OVERALL', overall=TRUE)

cross <- read.csv(csv.cross,  col.names=c('benchid','variant', 'jit', 'total', 'ratio'))
cross <- cross %>% mutate(overall=FALSE, variant=paste('Cross',variant, sep="\n")) 

shoot <- read.csv(csv.shoot,  col.names=c('benchid', 'jit', 'total', 'ratio'))
shoot <- shoot %>% mutate(variant='Shootout', overall=FALSE)

chap <- read.csv(csv.chap,  col.names=c('variant', 'benchid', 'jit', 'total', 'ratio'))
chap <- chap %>% mutate(overall=FALSE, variant=paste('Chaperones',variant, sep="\n")) 

plot.data <- rbind(cross,shoot,chap,overall)



######################################################

# ----- Outputting -----

p <- ggplot(data=plot.data,aes(x=variant,y=ratio)) + default.theme() +
  theme(
    plot.margin = unit(c(-3,3,-2,-0.5),"mm"),
    axis.text.x  = element_text(size=6,angle=0,hjust=0.5)
  ) +    
  geom_boxplot() + 
#   annotate("segment", x=0,xend=10,y = geomean(overall$ratio) , yend = geomean(overall$ratio), colour="green") +
  geom_jitter(height=.001, width=0, size=1, color="blue", alpha=0.5) +
  scale_y_continuous(breaks=seq(0,1,.2), limits=c(0,1),expand=c(0,0)) +
  ylab("Relative warmup time") +
  #   geom_point(position=position_jitter(width = 0.005, height=0), size=.25, color="blue") +
  facet_grid(. ~ overall, scales="free", space="free",labeller=label_bquote(""))
p
gg.file <- paste0(input.basename, ".pdf")
ggsave(gg.file, width=figure.width * ratio, height=figure.height, units=c("in"), colormodel='rgb')
embed_fonts(gg.file, options=pdf.embed.options)

print(">> slowest pycket")
print(head(overall %>% select(benchid,ratio,jit,total) %>% arrange(desc(ratio))))
print(">> fastest pycket")
print(head(overall %>% select(benchid,ratio,jit,total) %>% arrange(ratio)))

print(">> mean")
print(head(overall %>% summarize(mean(ratio))))
print(">> geomean")
print(head(overall %>% summarize(geomean(ratio))))


