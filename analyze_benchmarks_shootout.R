#! /usr/bin/env Rscript

# in inches
figure.width <- 7
ratio <- 2/3
figure.height <- 2.3


do.only.nothing <- TRUE
all.mean.in.graph <- TRUE
all.mean.in.graph.shootout <- FALSE
MAX.CROSS <- 3

reference.vm <- 'Racket'

blacklist <- c('strcat')

generic <- c('binarytrees-generic', 'fannkuch-redux-generic', 'fasta-generic', 'reversecomplement-generic', 'nbody-vec', 'nbody-generic', 'nbody-vec-generic', 'spectralnorm-generic', 'mandelbrot-generic')

tsv_name.default <- "output/Shootout.tsv"
"#
tsv_name.default <- 'output/20141112_e10b989_Shootout_pycket_fast.tsv'
tsv_name.default <- 'output/20150227_535ee83_Shootout.tsv'
"#

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


bench <- read.delim(tsv_name, comment.char = "#", header=FALSE,
                    col.names=c('timestamp', 'value', 'unit', 'criterion', 'benchmark', 'vm', 'suite', 'extra_args', 'warump', 'cores', 'input_size', 'variable_values'))

bench <- droplevels(bench[bench$criterion != 'gc',,])
bench$vm <- factor(bench$vm, levels = c("Pycket", "Racket"))  

# --- shaping data

bench <- droplevels(bench[!(bench$benchmark %in% blacklist),,])
bench <- bench[c('criterion','vm','benchmark','value', 'variable_values')]

num.vms <- length(levels(factor(bench$vm)))
num.runs <- length(bench$benchmark)
num.benches <- length(levels(bench$benchmark))
num.vars <- length(levels(factor(bench$variable_values)))
num.crit <- length(levels(bench$criterion))

if (num.vars == 0) {
  num.vars <- 1
}
rigorous <- num.runs > (num.vms * num.benches * num.vars * num.crit)

bench <- fill.missing(bench)


######################################################

bench.tot <- droplevels(bench[bench$criterion == 'total',,drop=TRUE])
bench.cpu <- droplevels(bench[bench$criterion == "cpu",,drop=TRUE])

group.by <- as.quoted(c("benchmark","vm"))

bench.summary <- summarize.bench(bench.tot, rigorous, FALSE, group.by, reference.vm)


bench.summary.graph <- droplevels(bench.summary[bench.summary$benchmark %ni% generic,,drop=TRUE])
bench.summary.generic <- droplevels(bench.summary[bench.summary$benchmark %in% generic,,drop=TRUE])
bench.summary.generic$benchmark <- gsub("-generic","\ngeneric",bench.summary.generic$benchmark)


# order by mean

bench.summary.graph$benchmark <- reorder(bench.summary.graph$benchmark, bench.summary.graph$mean.norm,
                                         function(x) {if (x[[1]] > 1.0) { max(x) } else { min(x) }})
bench.summary.generic$benchmark <- reorder(bench.summary.generic$benchmark, bench.summary.generic$mean.norm,
                                         function(x) {if (x[[1]] > 1.0) { max(x) } else { min(x) }})




all.mean.in.graph <- all.mean.in.graph.shootout

.selection <- c('vm','mean.norm')
.ids <- c('vm')
.group.by <- as.quoted(c("vm"))

bench.summary.overall.all <- ddply(melt(bench.summary[.selection], id.vars=.ids), .group.by,
                               plyr::summarize, 
                               overall=TRUE,
                               benchmark='geometric\nmean',
                               mean=1,
                               mean.norm=geomean(value),
                               median=1
)
print(">>>> OVERALL\n")
print(bench.summary.overall.all)

bench.summary.overall <- ddply(melt(bench.summary.graph[.selection], id.vars=.ids), .group.by,
                               plyr::summarize, 
                               overall=TRUE,
                               benchmark='geometric\nmean',
                               mean=1,
                               mean.norm=geomean(value),
                               median=1
)
if (rigorous) {
  bench.summary.overall.all <- merge(bench.summary.overall.all, data.frame(stdev=1, err095=1, cnfIntHigh=1,
                                                                       cnfIntLow=1, conf=1, upper=NA, lower=NA))
  
  
  bench.summary.overall <- merge(bench.summary.overall, data.frame(stdev=1, err095=1, cnfIntHigh=1,
                                                                   cnfIntLow=1, conf=1, upper=NA, lower=NA))
}

if (all.mean.in.graph) {
  bench.summary.graph <- rbind(bench.summary.graph, bench.summary.overall.all)
} else {
  bench.summary.graph <- rbind(bench.summary.graph, bench.summary.overall)  
}

sel.col = if (rigorous) { c('benchmark','vm','mean','err095') }  else 
                        { c('benchmark','vm','mean') }
bench.summary.sel <- dcast(melt(bench.summary[sel.col], id.vars=c('benchmark','vm')), benchmark ~ vm + variable)
bench.summary.ltx <- bench.summary.sel[2:length(bench.summary.sel)]
rownames(bench.summary.ltx) <- bench.summary.sel$benchmark
colnames(bench.summary.ltx) <- sapply(colnames(bench.summary.ltx), function(x) {sedit(x, '_', ' ')})

# ----- Outputting -----

# Excel for overall data
write.xlsx(bench.tot, paste0(input.basename, ".xlsx"), append=FALSE, sheetName="bench")
write.xlsx(bench.summary, paste0(input.basename, ".xlsx"), append=TRUE, sheetName="summary")

dodge <- position_dodge(width=.75)
ymax <- round_any(max(max(bench.summary.graph$mean.norm, na.rm=TRUE),
                      max(bench.summary.generic$mean.norm, na.rm=TRUE)), 0.5, ceiling)

# -------------------------------- Normal -----------------------------------------
p <- ggplot(data=bench.summary.graph,
       aes(x=benchmark,y=mean.norm,group=interaction(benchmark,vm),fill=vm,)
) + default.theme() +
  geom_bar(stat="identity", position=dodge, width=.6, aes(fill = vm))+
#   geom_point(position=dodge,aes(y=0.15, ymax=ymax, shape=vm),size=2, color="grey90",stat="identity") +
  ylab("Relative Runtime") +
  scale_y_continuous(breaks=seq(0,ymax,1), limits=c(0,ymax),expand=c(0,0)) +
  scale_fill_brewer(name = "Virtual Machine", type="qual", palette="Set1", guide="none") +
  facet_grid(. ~ overall, scales="free", space="free",labeller=label_bquote(""))
if (rigorous) {
  p <- p + geom_errorbar(aes(ymin=lower, ymax = upper),  position=dodge, color=I("black"), size=.2)  
}

p

gg.file <- paste0(input.basename, "-norm.pdf")
ggsave(gg.file, width=figure.width * ratio, height=figure.height, units=c("in"), colormodel='rgb')
embed_fonts(gg.file, options=pdf.embed.options)

# -------------------------------- Generic -----------------------------------------
p <- ggplot(data=bench.summary.generic,
            aes(x=benchmark,y=mean.norm,group=interaction(benchmark,vm),fill=vm,)
) + default.theme() +
  geom_bar(stat="identity", position=dodge, width=.6, aes(fill = vm))+
  #   geom_point(position=dodge,aes(y=0.15, ymax=ymax, shape=vm),size=2, color="grey90",stat="identity") +
  ylab("Relative Runtime") +
  scale_y_continuous(breaks=seq(0,ymax,1), limits=c(0,ymax),expand=c(0,0)) +
  scale_fill_brewer(name = "Virtual Machine", type="qual", palette="Set1", guide="none") +
  facet_grid(. ~ overall, scales="free", space="free",labeller=label_bquote(""))
if (rigorous) {
  p <- p + geom_errorbar(aes(ymin=lower, ymax = upper),  position=dodge, color=I("black"), size=.2)  
}

p

gg.file <- paste0(input.basename, "-generic-norm.pdf")
ggsave(gg.file, width=figure.width * (1 - ratio), height=figure.height, units=c("in"), colormodel='rgb')
embed_fonts(gg.file, options=pdf.embed.options)


if (rigorous) {
  # LaTeX table, all
  (function() {
    if (nrow(bench.summary.ltx) <= 0) return()
    len <- ncol(bench.summary.ltx)/2
    .just = rep(c('r','@{}>{\\smaller\\ensuremath{\\pm}}r@{\\,\\si{\\milli\\second}}'), len)
    .just = c('@{}r', .just[2:length(.just)])
    out <- latex(bench.summary.ltx,
                 file=paste0(input.basename, "-all.tex"),
                 rowlabel="Benchmark",
                 caption="All Shootout benchmarks results",
                 lines.page=999999,
                 booktabs=TRUE,
                 center="none",
                 longtable=TRUE,
                 size="small", #center="centering",
                 colheads=rep(c('mean', ''), len),
                 col.just=.just,
                 #col.just=rep(c('r','@{\\,\\si{\\milli\\second} \\ensuremath{\\pm}}r'), len),
                 cgroup=levels(as.factor(bench.summary$vm)),
                 cdec=rep(0, len*2))
  })()
  
} else {
  # LaTeX table, all
  (function() {
    if (nrow(bench.summary.ltx) <= 0) return()
    colnames(bench.summary.ltx) <- gsub(' mean', '', colnames(bench.summary.ltx))  
    len <- ncol(bench.summary.ltx)
    .just = rep('@{}r', len)
    .long <- nrow(bench.summary.ltx) > 50
    out <- latex(bench.summary.ltx,
                 file=paste0(input.basename, "-all.tex"),
                 rowlabel="Benchmark",
                 booktabs=TRUE,
                 lines.page=999999,
                 table.env=(! .long), center="none",
                 longtable=.long,
                 size="small", #center="centering",
                 col.just=.just,
                 cdec=rep(0, len))
  })()
}

bench.info <- bench.summary.graph[bench.summary.graph$overall == FALSE,,] 

pycket.timings <- (bench.info[bench.info$vm == 'Pycket',,])$mean.norm

specialization <- join(
  bench.summary %>% filter(benchmark %in% gsub("-generic", "", generic)) %>% filter(benchmark != 'nbody-vec') %>%  select(benchmark,vm,mean),
  bench.summary.generic %>% mutate(benchmark=gsub("\ngeneric","", benchmark), generic.mean=mean) %>% filter(benchmark != 'nbody-vec') %>% select(benchmark,vm,generic.mean),
  by = c('vm','benchmark')
) %>% mutate(slowdown=1-(1/(generic.mean/mean)))
print(ddply(specialization, .(vm), summarise, slowdown.geomean=geomean(slowdown),slowdown.mean=mean(slowdown)))



print(">> slowest pycket")
print(max(pycket.timings))
print(">> fastest pycket")
print(1/min(pycket.timings))

winners <- bench.info %>% group_by(benchmark) %>% filter(mean.norm == min(mean.norm))
winners <- winners[c('mean.norm','benchmark','vm')]
winners <- winners[do.call(order, winners),]
print("pycket wins in ");
print(winners[winners$vm == 'Pycket',,])
print("(this is")
print(nrow(winners[winners$vm == 'Pycket',,]))
print("out of")
print(nrow(winners))

print(">> done");
