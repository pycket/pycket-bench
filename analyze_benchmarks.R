#! /usr/bin/env Rscript

pkgs = c(
  "reshape2",
  "plyr",
  #"beanplot",
  #"boot",
  "Hmisc",
  "ggplot2",
  "tools",
  "extrafont",
  "xlsx"
)

use <- function(pkg) {
  if (!require(pkg, character.only=TRUE)) { install.packages(pkg) }
  library(pkg,character.only=TRUE)
}
sapply(pkgs, use)
loadfonts()

# ---- cmd line ----

if (FALSE) { 
  setwd("~/dev/pypy/pycket-bench")
}

if (length(commandArgs(trailingOnly=TRUE)) > 0) {
  tsv_name = commandArgs(trailingOnly=TRUE)[1]
} else {  
  # tsv_name <- "output/20140307-cache-envs.tsv"
  tsv_name <- "output/current.tsv"
}

if (!file.exists(tsv_name)) {
  stop("Cannot open input file ", tsv_name)
}

input.basename = file_path_sans_ext(tsv_name)

bench <- read.delim(tsv_name, comment.char = "#", header=FALSE,
                    col.names=c('timestamp', 'value', 'unit', 'criterion', 'benchmark', 'vm', 'suite', 'extra_args', 'warump', 'cores', 'input_size', 'variable_values'))

bench$vm <- sapply(bench$vm, function (x) 
  if (x=='RRacket') 'Racket' else paste0("",x))

# These are currently not run on pycket
blacklist <- c('gcold', 'graphs', 'mazefun', 'nucleic', 'parsing', 'string', 'sum1', 'wc')

# There are too big differences to plot. thus, table only

table.only <- c('ctak','fibc', 'paraffins')







# ------ functions -----
confInterval095Error <- function (samples) {
  if (length(sample) < 30)
    qnorm(0.975) * sd(samples) / sqrt(length(samples))
  else
    qt(0.975, df=length(samples)-1) * sd(samples) / sqrt(length(samples))
}

normalizeTo <- function(df, supergroup, group, val, var, vars=c(var)) {
  data <- df
  sg <- droplevels(df[[supergroup]])
  indexes <- which(df[[group]] == val)
  norm.factor <- (df[[var]])[indexes]
  names(norm.factor) <- sg[indexes]
  for (normvar in vars) {
    divis <- norm.factor[ sg ]
    res <- df[[normvar]]/divis
    data[[paste0(normvar,".norm")]] <- res
  }
  data
}

# --- shaping data


bench <- bench[!(bench$benchmark %in% blacklist),,drop=TRUE]
bench <- bench[c('criterion','vm','benchmark','value')]
bench.tot <- bench[bench$criterion == 'total',]
bench.cpu <- bench[bench$criterion == "cpu",]

bench.summary <- ddply(bench.tot, .(benchmark,vm), summarise, 
                       mean=mean(value),
                       median=median(value),
                       stdev=sd(value),
                       err095=confInterval095Error(value),
                       cnfIntHigh = mean(value) + (confInterval095Error(value)),
                       cnfIntLow = mean(value) - (confInterval095Error(value))
)

bench.summary <- normalizeTo(bench.summary, 'benchmark', 'vm', 'Racket', 'mean', c('mean', 'cnfIntHigh', 'cnfIntLow' ))

bench.tot.graph <- bench.tot[!(bench.tot$benchmark %in% table.only),]
bench.summary.graph <- ddply(bench.tot.graph, .(benchmark,vm), summarise, 
                        mean=mean(value),
                        median=median(value),
                        stdev=sd(value),
                        err095=confInterval095Error(value),
                        cnfIntHigh = mean(value) + (confInterval095Error(value)),
                        cnfIntLow = mean(value) - (confInterval095Error(value))
)
bench.summary.graph <- normalizeTo(bench.summary.graph, 'benchmark', 'vm', 'Racket', 'mean', c('mean', 'cnfIntHigh', 'cnfIntLow' ))

bench.tot.table <- subset(bench.tot, (benchmark %in% table.only))
bench.summary.table <- ddply(bench.tot.table, .(benchmark,vm), summarise, 
                        mean=mean(value),
                        median=median(value),
                        stdev=sd(value),
                        err095=confInterval095Error(value),
                        cnfIntHigh = mean(value) + (confInterval095Error(value)),
                        cnfIntLow = mean(value) - (confInterval095Error(value))
)

bench.summary.table.sel <- dcast(melt(bench.summary.table[c('benchmark','vm','mean','err095')], id.vars=c('benchmark','vm')), benchmark ~ vm + variable)
bench.summary.table.ltx <- bench.summary.table.sel[2:length(bench.summary.table.sel)]
rownames(bench.summary.table.ltx) <- bench.summary.table.sel$benchmark
colnames(bench.summary.table.ltx) <- sapply(colnames(bench.summary.table.ltx), function(x) {sedit(x, '_', ' ')})

# ----- Outputting -----

# Excel for overall data
write.xlsx(bench.tot, paste0(input.basename, ".xlsx"), append=FALSE, sheetName="bench")
write.xlsx(bench.summary, paste0(input.basename, ".xlsx"), append=TRUE, sheetName="summary")


# Normalized bargraph 1
pdf(paste0(input.basename, "-norm-shade.pdf"), height=4.3, width=7)
barplot(acast(bench.summary.graph, vm ~ benchmark, value.var="mean.norm", drop=TRUE),
        beside=TRUE,
        col=c("darkgreen","darkblue"), 
        density = c(30,10), 
        legend.text = TRUE,
        ylab = "Runtime normalized to Racket",
        ylim = c(0, 3.5),
        args.legend = list(x = "topleft"), las=2
)
dev.off()

# Normalized bargraph 2
dodge <- position_dodge(width=.8)
ggplot(data=bench.summary.graph, aes(
  x=benchmark,y=mean.norm,group=interaction(benchmark,vm),
  fill=vm,
)) +
  geom_bar(stat="identity", position=dodge, width=.75, aes(fill = vm),  )+
#   geom_errorbar(aes(ymin=cnfIntLow, ymax = cnfIntHigh),  position=dodge,color=I("black")) +
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
ggsave(paste0(input.basename, "-norm-col.pdf"), width=7, height=4.3, units=c("in"), colormodel='rgb')


# LaTeX table
out <- latex(bench.summary.table.ltx, file=paste0(input.basename, "-extremes.tex"),
      label="tbl:extremes",caption="Extreme runtimes",
      ctable=TRUE, 
      #booktabs=TRUE,
      where="htbp", size="footnotesize",
      #n.cgroup=rep(2, length(bench.summary.table.ltx)/2), 
      colheads=rep(c('mean', 'error'), length(bench.summary.table.ltx)/2),
      col.just=rep(c('r','@{ \\(\\pm\\)}r'), length(bench.summary.table.ltx)/2),
      cgroup=levels(as.factor(bench.summary.table$vm)), 
      cdec=rep(c(1,2), length(bench.summary.table.ltx)/2))

