if (!require(scales)) { install.packages("scales") }
if (!require(ggplot2)) { install.packages("ggplot2") }
if (!require(reshape)) { install.packages("reshape") }
if (!require(doBy)) { install.packages("doBy") }
if (!require(quantreg)) { install.packages("quantreg") }
if (!require(grid)) { install.packages("grid") }
if (!require(beanplot)) { install.packages("beanplot") }
library(scales)
library(ggplot2)
library(reshape)
library(doBy)
library(quantreg)
library(grid)
library(beanplot)
if (!require(extrafont)) { install.packages("extrafont") }
library(extrafont)
loadfonts()


confInterval095Error <- function (samples) {
  if (length(sample) < 30)
    qnorm(0.975) * sd(samples) / sqrt(length(samples))
  else
    qt(0.975, df=length(samples)-1) * sd(samples) / sqrt(length(samples))
}

if (FALSE) {
  setwd("~/dev/pypy/pycket-bench")
}
bench <- read.delim("output/current.tsv", comment.char = "#", header=FALSE,
                    col.names=c('timestamp', 'value', 'unit', 'criterion', 'benchmark', 'vm', 'suite', 'extra_args', 'warump', 'cores', 'input_size', 'variable_values'))

# These are currently not run on pycket
blacklist = c('gcold', 'graphs', 'mazefun', 'nucleic', 'parsing', 'string', 'sum1', 'wc')

bench <- subset(bench,
                !(benchmark %in% blacklist),
                select=c('criterion','vm','benchmark','value'))
bench.tot <- subset(bench, criterion == 'total')
bench.cpu <- subset(bench, criterion == "cpu")
#ddply(bench, .(criterion,vm,benchmark), transform, mean=mean(value),median=median(value),stdev=sd(value))
#bench.tot.ord <- orderBy(~benchmark+vm+variable_values, bench.tot)

# 
#beanplot(value/100~benchmark+vm,bench.tot, side = "both",border=NA,ll = 0.04, col = list("black", c("grey", "white")),what=c(1,1,1,1),log="y",overallline="median",beanlines="quantiles",bw="nrd0")

bench.tot1 <- subset(bench.tot, !(benchmark %in% c('ctak','fibc', 'paraffins')))
#beanplot(value~vm+benchmark,bench.tot1, side = "both",border=NA,ll = 0.04, col = list("black", c("grey", "white")),what=c(1,1,1,0),log="",overallline="median",bw="nrd0")
bench.summary <- ddply(bench.tot1, .(benchmark,vm), summarise, 
                      mean=mean(value),
                      median=median(value),
                      stdev=sd(value),
                      err095=confInterval095Error(value),
                      cnfIntHigh = mean(value) + (confInterval095Error(value)),
                      cnfIntLow = mean(value) - (confInterval095Error(value))
)

dodge <- position_dodge(width=.8)
ggplot(data=bench.summary, aes(
  x=benchmark,y=mean,group=interaction(benchmark,vm),
  fill=vm,
  )) +
   geom_bar(stat="identity", position=dodge, width=.75, aes(fill = vm),  )+
   geom_errorbar(aes(ymin=cnfIntLow, ymax = cnfIntHigh),  position=dodge,color=I("black")) +
  xlab("Benchmark") + ylab("Runtime") +
  theme_bw(base_size=9, base_family="Helvetica") +  
  theme(
       rect = element_rect(),
            axis.title.x = element_text(face="bold", size=11),
            axis.text.x  = element_text(size=9), #angle=45, vjust=0.2, 
            axis.title.y = element_text(face="bold", size=11),
            axis.text.y  = element_text(size=9), #angle=45, hjust=0.2, vjust=0.5, 
   legend.position=c(0.15, .90), 
    plot.margin = unit(c(0,3,0,-1),"mm"),
   legend.text = element_text(size=8),
   legend.title = element_text(size=8, face="bold"),
   legend.background = element_rect(fill="gray90", size=0), 
    legend.margin = unit(0, "cm"),
    legend.key=element_rect(fill="white"),
    legend.key.size=unit(5,"mm")
  ) +
  scale_fill_discrete(name = "Virtual Machine") +
  facet_null()
  
bench.speedup <- ddply(cast(bench.summary, benchmark ~ vm, value="mean"), 
                       .(benchmark), transform, Speedup=1-(Pycket/RRacket))

dodge <- position_dodge(width=.8)
ggplot(data=bench.speedup, aes(
  x=benchmark,y=Speedup)) +
  geom_bar(stat="identity", position=dodge, width=.75, fill=I("grey75"))+
#   geom_errorbar(aes(ymin=cnfIntLow, ymax = cnfIntHigh),  position=dodge,color=I("black")) +
#   xlab("Benchmark") + ylab("Runtime") +
  theme_bw(base_size=9, base_family="Helvetica") +  
  theme(
    rect = element_rect(),
    axis.title.x = element_text(face="bold", size=11),
    axis.text.x  = element_text(size=9), #angle=45, vjust=0.2, 
    axis.title.y = element_text(face="bold", size=11),
    axis.text.y  = element_text(size=9), #angle=45, hjust=0.2, vjust=0.5, 
    legend.position=c(0.15, .90), 
    plot.margin = unit(c(0,3,0,-1),"mm"),
    legend.text = element_text(size=8),
    legend.title = element_text(size=8, face="bold"),
    legend.background = element_rect(fill="gray90", size=0), 
    legend.margin = unit(0, "cm"),
    legend.key=element_rect(fill="white"),
    legend.key.size=unit(5,"mm")
  ) +
#   scale_fill_discrete(name = "Virtual Machine") +
  geom_hline(yintercept=0) +
  facet_null()


bench.tot2 <- subset(bench.tot, (benchmark %in% c('ctak','fibc', 'paraffins')))
bench.summary2 <- ddply(bench.tot2, .(benchmark,vm), summarise, 
                        mean=mean(value),
                        median=median(value),
                        stdev=sd(value),
                        err095=confInterval095Error(value),
                        cnfIntHigh = mean(value) + (confInterval095Error(value)),
                        cnfIntLow = mean(value) - (confInterval095Error(value))
)


ggplot(data=bench.summary2, aes(
  x=benchmark,y=mean,group=vm,
  color=vm,
  #   size=5
), position="dodge") +
  #geom_bar(stat="identity", position="dodge", fill="white") +
  geom_point()+
  geom_segment(aes(xend=benchmark,yend=0,ymax=mean), size=2,lineend = "square", position="dodge") +
  geom_errorbar(aes(ymin=cnfIntLow, ymax = cnfIntHigh)) +
  #   geom_boxplot() +
  #   geom_violin()+
  scale_y_log10() +
  #   geom_jitter() +
  xlab("Benchmark") + ylab("Runtime") +
  theme_bw(base_size=9, base_family="Helvetica") +  
  theme(
    #    rect = element_rect(),
    #         axis.title.x = element_text(face="bold", size=11),
    #         axis.text.x  = element_text(size=9), #angle=45, vjust=0.2, 
    #         axis.title.y = element_text(face="bold", size=11),
    #         axis.text.y  = element_text(size=9), #angle=45, hjust=0.2, vjust=0.5, 
    legend.position=c(0.15, .5), 
    plot.margin = unit(c(0,3,0,-1),"mm"),
    legend.background = element_rect(fill="gray90", size=0), 
    legend.margin = unit(0, "cm"),
    legend.key=element_rect(fill="white"),
    legend.key.size=unit(5,"mm")
    #         legend.text = element_text(size=8),
    #         legend.title = element_text(size=8, face="bold")
  ) +
  facet_null()

