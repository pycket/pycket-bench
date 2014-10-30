#! /usr/bin/env Rscript

pkgs = c(
  "reshape2",
  "plyr",
  #"beanplot",
  "boot",
  "Hmisc",
  "ggplot2",
  "tools",
  "xlsx"
)

use <- function(pkg) {
  if (!require(pkg, character.only=TRUE)) { install.packages(pkg) }
  library(pkg,character.only=TRUE)
}
sapply(pkgs, use)
if (!require(extrafont)) {
  install.packages("devtools")
  library(devtools)
  install_github("Rttf2pt1", "wch")
  install_github("extrafont", "wch")
}
library(extrafont)
loadfonts(quiet=TRUE)
if (length(fonts()) == 0) {
  font_import(prompt=FALSE)
}

pdf.embed.options <- "-dEmbedAllFonts=true -dPDFSETTINGS=/prepress -dCompatibilityLevel=1.4 -dSubsetFonts=true -dHaveTrueTypeFonts=true"

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
                                
bench <- droplevels(bench[bench$vm != 'PycketNoJit',,drop=TRUE])
bench <- droplevels(bench[bench$criterion != 'gc',,drop=TRUE])
if (length(factor(bench$vm)) > 2) {
  bench$vm <- factor(bench$vm, levels = c("Pycket", "Racket", "Larceny", "Gambit", "Bigloo"))
} else {
  bench$vm <- factor(bench$vm, levels = c("Pycket", "Racket"))  
}

reference.vm <-  if ('Racket' %in% bench$vm) 'Racket' else 'Pycket'

# These are currently not run on pycket
blacklist <- c()

# There are too big differences to plot. thus, table only
if ('ctak' %in% bench$benchmark) {  
  table.only <- c('ctak','fibc', 'pi', 'nucleic', 'dynamic', 'cat', 'maze', 'slatex', 'matrix')
  #table.only <- c('ctak')
} else {
  table.only <- c()
}

# in inches
figure.width <- 7
figure.height <- 2.8






# ------ functions -----
confInterval095Error <- function (samples) {
  if (length(sample) < 30)
    qnorm(0.975) * sd(samples) / sqrt(length(samples))
  else
    qt(0.975, df=length(samples)-1) * sd(samples) / sqrt(length(samples))
}

div_mean_x_y = function(data, indices, extraid) {
    indexx = indices[extraid == "x"]
    indexy = indices[extraid == "y"]
    mean(data[indexx]) / mean(data[indexy])
}

normalize_value_bootstrap_confidence = function(x, y, R=1000) {
    # x and y need to be *vectors* of all the values
    # the result is the confidence interval of mean(x) / mean(y)
    total <- c(x,y)
    id <- as.factor(c(rep("x",length(x)),rep("y",length(y))))

    b <- boot(total, div_mean_x_y, strata=id, R=R, extraid=id)
    norm <- boot.ci(b, type=c("norm"))$normal
    dimnames(norm) <- list(NULL,c("conf", "upper", "lower"))
    norm
}

bootstrapTo <- function(df, supergroup, group, val, var) {
  cmp <- df[(df[[group]] == val),]
  doIt <- function(X) {
    ident <- (X[[supergroup]])[[1]]
    subs <- cmp[(cmp[[supergroup]] == ident),]
    comparee <- subs[[var]]
    normalize_value_bootstrap_confidence(X[[var]], comparee)
  }
  ddply(df, c(supergroup,group), doIt)  
}

normalizeTo <- function(df, supergroup, group, val, var, vars=c(var)) {
  data <- df
  sg <- droplevels(df[[supergroup]])
  indexes <- which(df[[group]] == val)
  norm.factor <- (df[[var]])[indexes]
  names(norm.factor) <- droplevels(sg[indexes])
  for (normvar in vars) {
    divis <- norm.factor[ sg ]
    res <- df[[normvar]]/divis
    data[[paste0(normvar,".norm")]] <- res
  }
  data
}

# --- shaping data


bench <- droplevels(bench[!(bench$benchmark %in% blacklist),,drop=TRUE])
bench <- bench[c('criterion','vm','benchmark','value', 'variable_values')]

bench.corr = data.frame()
for (var in levels(bench$variable_values)) {
  for (crit in levels(bench$criterion)) {
    for (vm in levels(bench$vm)) {  
      for (benchmark in levels(bench$benchmark)) {
        if (1 > nrow(bench[(
          bench$variable_values == var &
            bench$criterion == crit & 
            bench$vm == vm &
            bench$benchmark == benchmark
          ),,])) {
          .x <- data.frame(criterion=crit,vm=vm,benchmark=benchmark,value=NA,variable_values=var)
          bench.corr <- rbind(bench.corr, .x)
        }
      }
    }
  }
}

bench <- rbind(bench, bench.corr)

bench.tot <- droplevels(bench[bench$criterion == 'total',,drop=TRUE])
bench.cpu <- droplevels(bench[bench$criterion == "cpu",,drop=TRUE])

num.vms <- length(levels(factor(bench.tot$vm)))
num.runs <- length(bench.tot$benchmark)
num.benches <- length(levels(bench.tot$benchmark))
num.vars <- length(levels(factor(bench.tot$variable_values)))

rigorous <- (num.runs / num.vms) > (num.benches * num.vars)
multi.variate <- num.vars > 1

if (multi.variate) {
  group.by <- as.quoted(c("variable_values","benchmark","vm"))
} else {
  group.by <- as.quoted(c("benchmark","vm"))
}

if (rigorous) {
  #fixme: mv
  bench.err <- bootstrapTo(bench.tot, 'benchmark', 'vm', 'Racket', 'value')

  bench.summary <- ddply(bench.tot, group.by, plyr::summarize, 
                         overall=FALSE,
                         mean=mean(value),
                         median=median(value),
                         stdev=sd(value),
                         err095=confInterval095Error(value),
                         cnfIntHigh = mean(value) + (confInterval095Error(value)),
                         cnfIntLow = mean(value) - (confInterval095Error(value))
  )
  bench.summary <- merge(
    normalizeTo(bench.summary, 'benchmark', 'vm', reference.vm, 'mean'),
    bench.err
  )
} else {
  bench.summary <- ddply(bench.tot, group.by, plyr::summarize, 
                         overall=FALSE,
                         mean=mean(value),
                         median=median(value)
  )
  if (multi.variate) {
    df <- data.frame()
    for (var.val in levels(factor(bench.summary$variable_values))) {
      b <- droplevels(bench.summary[bench.summary$variable_values == var.val,,drop=TRUE])
      df <- rbind(df, normalizeTo(b, 'benchmark', 'vm', reference.vm, 'mean'))
    }
    bench.summary <- df
  } else {
    bench.summary <- normalizeTo(bench.summary, 'benchmark', 'vm', reference.vm, 'mean')
  }
}


bench.summary.graph <- droplevels(bench.summary[!(bench.summary$benchmark %in% table.only),,drop=TRUE])
# ignore nojit
#bench.summary.graph <- bench.summary.graph[bench.summary.graph$vm != 'PycketNoJit',,drop=TRUE]

# bench.summary.overall <- ddply(melt(bench.summary.graph[c('vm','mean.norm')], id.vars=c('vm')), .(vm),
#                                plyr::summarize, 
#                                overall=TRUE,
#                                benchmark='geometric\nmean',
#                                mean=1,
#                                mean.norm=exp(mean(log(value))),
#                                median=1
# )
# if (rigorous) {
#   bench.summary.overall <- merge(bench.summary.overall, data.frame(stdev=1, err095=1, cnfIntHigh=1,
#                                                                    cnfIntLow=1, conf=1, upper=NA, lower=NA))
# 
# }
# bench.summary.graph <- rbind(bench.summary.graph, bench.summary.overall)
#bench.summary.graph <- normalizeTo(bench.summary.graph, 'benchmark', 'vm', 'Racket', 'mean', c('mean', 'cnfIntHigh', 'cnfIntLow' ))

sel.col = if (rigorous) { if (multi.variate) { c('variable_values','benchmark','vm','mean','err095') } else { c('benchmark','vm','mean','err095') }  } else 
                        { if (multi.variate) { c('variable_values','benchmark','vm','mean') } else { c('benchmark','vm','mean')} }
if (multi.variate) {
  bench.summary.sel <- dcast(melt(bench.summary[sel.col], id.vars=c('variable_values','benchmark','vm')), variable_values + benchmark ~ vm + variable)
} else {
  bench.summary.sel <- dcast(melt(bench.summary[sel.col], id.vars=c('benchmark','vm')), benchmark ~ vm + variable)
  bench.summary.ltx <- bench.summary.sel[2:length(bench.summary.sel)]
  rownames(bench.summary.ltx) <- bench.summary.sel$benchmark
  colnames(bench.summary.ltx) <- sapply(colnames(bench.summary.ltx), function(x) {sedit(x, '_', ' ')})
}

bench.summary.table <- droplevels(bench.summary[bench.summary$vm != 'PycketNoJit',,drop=TRUE])
if (multi.variate) {
  bench.summary.table.sel <- dcast(melt(bench.summary.table[sel.col], id.vars=c('variable_values','benchmark','vm')), variable_values + benchmark ~ vm + variable)  
} else {
  bench.summary.table.sel <- dcast(melt(bench.summary.table[sel.col], id.vars=c('benchmark','vm')), benchmark ~ vm + variable)
  bench.summary.table.ltx <- bench.summary.table.sel[2:length(bench.summary.table.sel)]
  rownames(bench.summary.table.ltx) <- bench.summary.table.sel$benchmark
  colnames(bench.summary.table.ltx) <- sapply(colnames(bench.summary.table.ltx), function(x) {sedit(x, '_', ' ')})
  bench.summary.table.ltx <- bench.summary.table.ltx[rownames(bench.summary.table.ltx) %in% table.only,]
}

# ----- Outputting -----

# Excel for overall data
write.xlsx(bench.tot, paste0(input.basename, ".xlsx"), append=FALSE, sheetName="bench")
write.xlsx(bench.summary, paste0(input.basename, ".xlsx"), append=TRUE, sheetName="summary")

# 
# # Normalized bargraph 1
# pdf(paste0(input.basename, "-norm-shade.pdf"), height=figure.height, width=figure.width)
# barplot(acast(bench.summary.graph, vm ~ benchmark, value.var="mean.norm", drop=TRUE),
#         beside=TRUE,
#         col=c("darkgreen","darkblue"),
#         density = c(30,10),
#         legend.text = TRUE,
#         ylab = "Runtime (normalized)",
#         ylim = c(0, 3.5),
#         #args.legend = list(x = "topleft"),
#         las=2
# )
# dev.off()

# Normalized bargraph
dodge <- position_dodge(width=.8)
#ymax <- round_any(max(1/bench.summary.graph$mean.norm,  na.rm=TRUE), 0.5, ceiling)
ymax <- round_any(max(bench.summary.graph$mean.norm, na.rm=TRUE), 0.5, ceiling)
p <- ggplot(data=bench.summary.graph,
#        aes(x=benchmark,y=1/mean.norm,group=interaction(benchmark,vm),fill=vm,)
       aes(x=benchmark,y=mean.norm,group=interaction(benchmark,vm),fill=vm,)
) +
  geom_bar(stat="identity", position=dodge, width=.75, aes(fill = vm))+
  geom_point(position=dodge,aes(y=0.2, ymax=ymax, shape=vm),size=2, color="grey90",stat="identity") +
  #   xlab("Benchmark") +
  ylab("Relative Runtime") +
  theme_bw(base_size=8, base_family="Helvetica") +
  theme(
    rect = element_rect(),
    axis.title.x =  element_blank(),
    #     axis.title.x = element_text(face="bold", size=9),
    #     axis.text.x  = element_text(size=9), #angle=45, vjust=0.2,
    axis.text.x  = element_text(size=8, angle=45, hjust=1),
    axis.title.y = element_text(face="bold", size=8),
    axis.text.y  = element_text(size=8), #angle=45, hjust=0.2, vjust=0.5,
    legend.position=c(0.15, .75),
    #plot.margin = unit(c(-3.2,3,-4,-1),"mm"),
    legend.text = element_text(size=7),
    legend.title = element_text(size=7, face="bold"),
    legend.background = element_rect(fill="gray90", size=0),
    legend.margin = unit(0, "cm"),
    legend.key=element_rect(fill="white"),
    legend.key.size=unit(5,"mm")
  ) +
  scale_y_continuous(breaks=seq(0,ymax,1), limits=c(0,ymax),expand=c(0,0)) +
  #scale_fill_grey(name = "Virtual Machine")
  scale_fill_brewer(name = "Virtual Machine", type="qual", palette="Set1") +
  scale_shape(name = "Virtual Machine", solid = FALSE)
if (multi.variate) {
  p <- p + facet_grid(variable_values ~ .)#, scales="free", space="free",labeller=label_bquote(""))
} else {
  p <- p + facet_null()
  #p <- p + facet_grid(. ~ overall, scales="free", space="free",labeller=label_bquote(""))
}
if (rigorous) {
  p <- p + geom_errorbar(aes(ymin=lower, ymax = upper),  position=dodge, color=I("black"), size=.33)  
}

p

gg.file <- paste0(input.basename, "-norm.pdf")
#ggsave(gg.file, width=figure.width, height=figure.height*num.vars, units=c("in"), colormodel='rgb')
ggsave(gg.file, width=20, height=7, units=c("in"), colormodel='rgb')
embed_fonts(gg.file, options=pdf.embed.options)


if (FALSE) {
#
# and now color
#
p <- ggplot(data=bench.summary.graph,
#        aes(x=benchmark,y=1/mean.norm,group=interaction(benchmark,vm),fill=vm,)
       aes(x=benchmark,y=mean.norm,group=interaction(benchmark,vm),fill=vm,)
) +
  geom_bar(stat="identity", position=dodge, width=.75, aes(fill = vm),  )+
  #   xlab("Benchmark") +
  ylab("Relative Runtime") +
  theme_bw(base_size=8, base_family="Helvetica") +
  theme(
    rect = element_rect(),
    axis.title.x =  element_blank(),
    #     axis.title.x = element_text(face="bold", size=9),
    #     axis.text.x  = element_text(size=9), #angle=45, vjust=0.2,
    axis.text.x  = element_text(size=8, angle=45, hjust=1),
    axis.title.y = element_text(face="bold", size=8),
    axis.text.y  = element_text(size=8), #angle=45, hjust=0.2, vjust=0.5,
    legend.position=c(0.15, .75),
    plot.margin = unit(c(-3.2,3,-4,-1),"mm"),
    legend.text = element_text(size=7),
    legend.title = element_text(size=7, face="bold"),
    legend.background = element_rect(fill="gray90", size=0),
    legend.margin = unit(0, "cm"),
    legend.key=element_rect(fill="white"),
    legend.key.size=unit(5,"mm")
  ) +
  scale_y_continuous(breaks=seq(0,ymax,.5), limits=c(0,ymax),expand=c(0,0)) +
  scale_fill_brewer(name = "Virtual Machine", type="qual", palette="Set1") +
  #facet_null()
  facet_grid(. ~ overall, scales="free", space="free",labeller=label_bquote(""))
if (rigorous) {
  p <- p + geom_errorbar(aes(ymin=lower, ymax = upper),  position=dodge, color=I("black"), size=.33)  
}
p
gg.file <- paste0(input.basename, "-norm-col.pdf")
ggsave(gg.file, width=figure.width, height=figure.height, units=c("in"), colormodel='rgb')
embed_fonts(gg.file, options=pdf.embed.options)

}

# LaTeX table
(function() {
  len <- length(bench.summary.table.ltx)/2
  out <- latex(bench.summary.table.ltx,
               file=paste0(input.basename, "-extremes.tex"),
               rowlabel="",
               #label="tbl:extremes",caption="Extreme runtimes",
               booktabs=TRUE,
               #ctable=TRUE,
               cgroupTexCmd="mdseries",
               rgroupTexCmd="mdseries",
               table.env=FALSE, center="none",
               where="htbp", size="footnotesize", #center="centering",
               #colheads=rep(c('mean', 'error'), len),
               colheads=rep('',len*2),
               col.just=rep(c('r','@{\\scriptsize\\,\\ensuremath{\\pm}}>{\\scriptsize}r'), len),
               cgroup=levels(as.factor(bench.summary.table$vm)),
               cdec=rep(0, len*2))
})()
# LaTeX table, all
(function() {
  len <- length(bench.summary.ltx)/2
  out <- latex(bench.summary.ltx,
               file=paste0(input.basename, "-all.tex"),
               rowlabel="Benchmark",
               booktabs=TRUE,
               table.env=FALSE, center="none",
               size="footnotesize", #center="centering",
               colheads=rep(c('mean', 'error'), len),
               col.just=rep(c('r','@{\\,\\si{\\milli\\second} \\ensuremath{\\pm}}r'), len),
               cgroup=levels(as.factor(bench.summary$vm)),
               cdec=rep(0, len*2))
})()
