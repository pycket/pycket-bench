#! /usr/bin/env Rscript

# in inches
figure.width <- 7
figure.height <- 2.8

do.only.nothing <- TRUE
all.mean.in.graph <- TRUE



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

use <- function(pkg) {
  if (!require(pkg, character.only=TRUE)) { install.packages(pkg, repos="http://cran.rstudio.com") }
  library(pkg,character.only=TRUE)
}
sapply(pkgs, use)
if (!require(extrafont)) {
  install.packages("devtools", repos="http://cran.rstudio.com")
  library(devtools)
  install_github("wch/Rttf2pt1", ref='21c5dbc42bedf1714cc11c8a3c0d5086d4e7b85f')
  install_github("wch/extrafont")
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

if ('Racket' %in% bench$vm) {
  if (length(factor(bench$vm)) > 2) {
    bench$vm <- factor(bench$vm, levels = c("Pycket", "Racket", "Larceny", "Gambit", "Bigloo"))
  } else {
    bench$vm <- factor(bench$vm, levels = c("Pycket", "Racket"))  
  }
  # There are too big differences to plot. thus, table only
  if ('ctak' %in% bench$benchmark) {  
    table.only <- c('ctak','fibc', 'pi', 'nucleic')
    #table.only <- c('ctak')
  } else if ('fannkuch-redux' %in% bench$benchmark) {
    # these are not the original ones, ignore them
    table.only <- c('binarytrees-generic', 'fannkuch-redux-generic', 'fasta-generic', 'reversecomplement-generic')
  }
}
reference.vm <-  if ('Racket' %in% bench$vm) 'Racket' else 'Pycket'

# These are currently not run on pycket
blacklist <- c('sum1', 'wc', 'cat', 'slatex')


message(">>>>>>>>>> Putting these only into a table: ")
message(table.only)





# ------ functions -----
`%ni%` = Negate(`%in%`) 


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
    .X <- X[(!is.na(X[[var]])),]
    if (1 > nrow(.X)) {
      return(as.matrix(data.frame(conf=NA, upper=NA, lower=NA)))
    }
    ident <- (X[[supergroup]])[[1]]
    subs <- cmp[(cmp[[supergroup]] == ident),]
    comparee <- subs[[var]]
#     print(X[[group]][[1]])
#     print(ident)
#     print(is.na(comparee))
    if (length(comparee[!is.na(comparee)]) != length(comparee)) {
      return(as.matrix(data.frame(conf=NA, upper=NA, lower=NA)))
    }
    normalize_value_bootstrap_confidence(.X[[var]], comparee)
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

geomean <- function(X) {
  value <- na.omit(X[X > 0])
  exp(mean(log(value)))
}


# --- shaping data






bench <- droplevels(bench[!(bench$benchmark %in% blacklist),,drop=TRUE])
bench <- bench[c('criterion','vm','benchmark','value', 'variable_values')]

if ('ackermann' %in% bench$benchmark & 'cpstak' %in% bench$benchmark) {
  multi.variate <- FALSE
  rigorous <- FALSE  
} else {
  num.vms <- length(levels(factor(bench$vm)))
  num.runs <- length(bench$benchmark)
  num.benches <- length(levels(bench$benchmark))
  num.vars <- length(levels(factor(bench$variable_values)))
  num.crit <- length(levels(bench$criterion))
  
  if (num.vars == 0) {
    num.vars <- 1
  }
  rigorous <- num.runs > (num.vms * num.benches * num.vars * num.crit)
  multi.variate <- num.vars > 1

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
            if (rigorous) {
              bench.corr <- rbind(bench.corr, .x)
              bench.corr <- rbind(bench.corr, .x)
              bench.corr <- rbind(bench.corr, .x)
              bench.corr <- rbind(bench.corr, .x)
              bench.corr <- rbind(bench.corr, .x)
              bench.corr <- rbind(bench.corr, .x)
              bench.corr <- rbind(bench.corr, .x)            
              bench.corr <- rbind(bench.corr, .x)
              bench.corr <- rbind(bench.corr, .x)            
              
            }
          }
        }
      }
    }
  }
  bench <- rbind(bench, bench.corr)
}


######################################################

bench.tot <- droplevels(bench[bench$criterion == 'total',,drop=TRUE])
bench.cpu <- droplevels(bench[bench$criterion == "cpu",,drop=TRUE])

if (multi.variate) {
  group.by <- as.quoted(c("variable_values","benchmark","vm"))
} else {
  group.by <- as.quoted(c("benchmark","vm"))
}

if (rigorous) {
  bench.summary <- ddply(bench.tot, group.by, plyr::summarize, 
                         overall=FALSE,
                         mean=mean(value),
                         median=median(value),
                         stdev=sd(value),
                         err095=confInterval095Error(value),
                         cnfIntHigh = mean(value) + (confInterval095Error(value)),
                         cnfIntLow = mean(value) - (confInterval095Error(value))
  )
  if (multi.variate) {
    df <- data.frame()
    for (var.val in levels(factor(bench.summary$variable_values))) {
      .sum <- droplevels(bench.summary[bench.summary$variable_values == var.val,,drop=TRUE])
      .tot <- droplevels(bench.tot[bench.tot$variable_values == var.val,,drop=TRUE])
      .err <- bootstrapTo(.tot, 'benchmark', 'vm', reference.vm, 'value')
      .norm <- normalizeTo(.sum, 'benchmark', 'vm', reference.vm, 'mean')
      .merge <- merge(.norm, .err)
      df <- rbind(df, .merge)
    }
    bench.summary <- df
  } else {
    bench.err <- bootstrapTo(bench.tot, 'benchmark', 'vm', reference.vm, 'value')
    bench.summary <- merge(
      normalizeTo(bench.summary, 'benchmark', 'vm', reference.vm, 'mean'),
      bench.err
    )
  }
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

if (multi.variate) {
  .selection <- c('vm', 'variable_values','mean.norm')
  .ids <- c('vm', 'variable_values')
  .group.by <- as.quoted(c("vm","variable_values"))
  
} else {
  .selection <- c('vm','mean.norm')
  .ids <- c('vm')
  .group.by <- as.quoted(c("vm"))
}

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
#bench.summary.graph <- normalizeTo(bench.summary.graph, 'benchmark', 'vm', 'Racket', 'mean', c('mean', 'cnfIntHigh', 'cnfIntLow' ))

sel.col = if (rigorous) { if (multi.variate) { c('variable_values','benchmark','vm','mean','err095') } else { c('benchmark','vm','mean','err095') }  } else 
                        { if (multi.variate) { c('variable_values','benchmark','vm','mean') } else { c('benchmark','vm','mean')} }
if (multi.variate) {
  bench.summary.sel <- dcast(melt(bench.summary[sel.col], id.vars=c('benchmark','variable_values','vm')), benchmark + variable_values ~ vm + variable)
  bench.summary.ltx <- bench.summary.sel[3:length(bench.summary.sel)]
  rownames(bench.summary.ltx) <- paste0(bench.summary.sel$benchmark, '\\textsubscript{', bench.summary.sel$variable_values, '}')
  colnames(bench.summary.ltx) <- sapply(colnames(bench.summary.ltx), function(x) {sedit(x, '_', ' ')})
} else {
  bench.summary.sel <- dcast(melt(bench.summary[sel.col], id.vars=c('benchmark','vm')), benchmark ~ vm + variable)
  bench.summary.ltx <- bench.summary.sel[2:length(bench.summary.sel)]
  rownames(bench.summary.ltx) <- bench.summary.sel$benchmark
  colnames(bench.summary.ltx) <- sapply(colnames(bench.summary.ltx), function(x) {sedit(x, '_', ' ')})
}

bench.summary.table <- droplevels(bench.summary[bench.summary$vm != 'PycketNoJit',,drop=TRUE])
if (multi.variate) {
  bench.summary.table.sel <- dcast(melt(bench.summary.table[sel.col], id.vars=c('benchmark','variable_values','vm')), benchmark + variable_values ~ vm + variable)  
  bench.summary.table.sel.flt <- bench.summary.table.sel[bench.summary.table.sel$benchmark %in% table.only,]
  bench.summary.table.ltx <- bench.summary.table.sel.flt[3:length(bench.summary.table.sel.flt)]
  rownames(bench.summary.table.ltx) <- paste0(bench.summary.table.sel.flt$benchmark, '\\textsubscript{', bench.summary.table.sel.flt$variable_values, '}')
  colnames(bench.summary.table.ltx) <- sapply(colnames(bench.summary.table.ltx), function(x) {sedit(x, '_', ' ')})
} else {
  bench.summary.table.sel <- dcast(melt(bench.summary.table[sel.col], id.vars=c('benchmark','vm')), benchmark ~ vm + variable)
  bench.summary.table.sel <- bench.summary.table.sel[bench.summary.table.sel$benchmark %in% table.only,]
  bench.summary.table.ltx <- bench.summary.table.sel[2:length(bench.summary.table.sel)]
  rownames(bench.summary.table.ltx) <- bench.summary.table.sel$benchmark
  colnames(bench.summary.table.ltx) <- sapply(colnames(bench.summary.table.ltx), function(x) {sedit(x, '_', ' ')})
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

if (multi.variate & do.only.nothing) {
  bench.summary.graph <- droplevels(bench.summary.graph[bench.summary.graph$variable_values == 'nothing',,drop=TRUE])
}

if ('ackermann' %in% bench$benchmark & 'cpstak' %in% bench$benchmark) {
  
  # Normalized bargraph
  dodge <- position_dodge(width=.8)
  #ymax <- round_any(max(1/bench.summary.graph$mean.norm,  na.rm=TRUE), 0.5, ceiling)
  ymax <- round_any(max(bench.summary.overall.all$mean.norm, na.rm=TRUE), 1, ceiling)
  p <- ggplot(data=bench.summary.overall.all,
              #        aes(x=benchmark,y=1/mean.norm,group=interaction(benchmark,vm),fill=vm,)
              aes(x=benchmark,y=mean.norm,group=interaction(benchmark,vm),fill=vm,)
  ) +
    geom_bar(stat="identity", position=dodge, width=.75, aes(fill = vm))+
#     geom_point(position=dodge,aes(y=0.15, ymax=ymax, shape=vm),size=2, color="grey90",stat="identity") +
    xlab("") +
    ylab("Relative Runtime") +
    theme_bw(base_size=8, base_family="Helvetica") +
    theme(
       rect = element_rect(),
      axis.title.x =  element_blank(),
#       #     axis.title.x = element_text(face="bold", size=9),
#       #     axis.text.x  = element_text(size=9), #angle=45, vjust=0.2,
#       axis.text.x  = element_text(size=8, angle=45, hjust=1),
#       axis.title.y = element_text(face="bold", size=8),
#       axis.text.y  = element_text(size=8), #angle=45, hjust=0.2, vjust=0.5,
      legend.position=c(0.5, .85),
#       legend.position = "bottom"
#       #plot.margin = unit(c(-3.2,3,-4,-1),"mm"),
#       legend.text = element_text(size=7),
#       legend.title = element_text(size=7, face="bold"),
#       legend.background = element_rect(fill="gray90", size=0),
#       legend.margin = unit(0, "cm"),
#       legend.key=element_rect(fill="white"),
       legend.key.size=unit(3,"mm")
    ) +
    scale_y_continuous(breaks=seq(0,ymax,.2), limits=c(0,ymax),expand=c(0,0)) +
    #scale_fill_grey(name = "Virtual Machine")
    scale_fill_brewer(name = "Virtual Machine", type="qual", palette="Set2") 
#+
#    scale_shape_manual(name = "Virtual Machine", values=c(1,8,10,13))
  if (rigorous) {
    p <- p + geom_errorbar(aes(ymin=lower, ymax = upper),  position=dodge, color=I("black"), size=.33)  
  }
  p <- p + facet_null()
  
  p
  figure.width  <- figure.width / 3

} else {
  

# Normalized bargraph
dodge <- position_dodge(width=.8)
#ymax <- round_any(max(1/bench.summary.graph$mean.norm,  na.rm=TRUE), 0.5, ceiling)
ymax <- round_any(max(bench.summary.graph$mean.norm, na.rm=TRUE), 0.5, ceiling)
p <- ggplot(data=bench.summary.graph,
#        aes(x=benchmark,y=1/mean.norm,group=interaction(benchmark,vm),fill=vm,)
       aes(x=benchmark,y=mean.norm,group=interaction(benchmark,vm),fill=vm,)
) +
  geom_bar(stat="identity", position=dodge, width=.75, aes(fill = vm))+
#   geom_point(position=dodge,aes(y=0.15, ymax=ymax, shape=vm),size=2, color="grey90",stat="identity") +
  #   xlab("Benchmark") +
  ylab("Relative Runtime") +
  theme_bw(base_size=6, base_family="Helvetica") +
  theme(
    rect = element_rect(),
    axis.title.x =  element_blank(),
    #     axis.title.x = element_text(face="bold", size=9),
    #     axis.text.x  = element_text(size=9), #angle=45, vjust=0.2,
    axis.text.x  = element_text(size=6, angle=45, hjust=1),
    axis.title.y = element_text(face="bold", size=6),
    axis.text.y  = element_text(size=6), #angle=45, hjust=0.2, vjust=0.5,
    legend.position=c(0.15, .75),
    plot.margin = unit(c(-3.2,3,-4,-1),"mm"),
    legend.text = element_text(size=6),
    legend.title = element_text(size=6, face="bold"),
    legend.background = element_rect(fill="gray90", size=0),
    legend.margin = unit(0, "cm"),
    legend.key=element_rect(fill="white"),
    legend.key.size=unit(3,"mm")
  )
if ('fannkuch-redux' %in% bench$benchmark) {
  p <- p +
    scale_y_continuous(breaks=seq(0,ymax,1), limits=c(0,ymax),expand=c(0,0)) +
    #scale_fill_grey(name = "Virtual Machine", guide="none")
    scale_fill_brewer(name = "Virtual Machine", type="qual", palette="Set1", guide="none")
} else {
  p <- p +
  scale_y_continuous(breaks=seq(0,ymax,1), limits=c(0,ymax),expand=c(0,0)) +
  #scale_fill_grey(name = "Virtual Machine")
  scale_fill_brewer(name = "Virtual Machine", type="qual", palette="Set1")
#   + scale_shape(name = "Virtual Machine", solid = FALSE)
}
if (rigorous) {
  p <- p + geom_errorbar(aes(ymin=lower, ymax = upper),  position=dodge, color=I("black"), size=.33)  
}


if (multi.variate & !do.only.nothing) {
  .labeller <- function(var, value) {
    if (var == 'overall') {
      label_bquote("")(var, value)
    } else {
      label_value(var, value)
    }  
  }
#   p <- p + facet_grid(variable_values ~ .)#, scales="free", space="free",labeller=label_bquote(""))
    p <- p + facet_grid(variable_values ~ overall, scales="free_x", space="free_x",labeller=.labeller)
  
} else {
  #p <- p + facet_null()
  p <- p + facet_grid(. ~ overall, scales="free", space="free",labeller=label_bquote(""))
}

p
}

gg.file <- paste0(input.basename, "-norm.pdf")
ggsave(gg.file, width=figure.width, height=figure.height, units=c("in"), colormodel='rgb')
#ggsave(gg.file, width=20, height=7, units=c("in"), colormodel='rgb')
embed_fonts(gg.file, options=pdf.embed.options)


# if (FALSE) {
# #
# # and now color
# #
# p <- ggplot(data=bench.summary.graph,
# #        aes(x=benchmark,y=1/mean.norm,group=interaction(benchmark,vm),fill=vm,)
#        aes(x=benchmark,y=mean.norm,group=interaction(benchmark,vm),fill=vm,)
# ) +
#   geom_bar(stat="identity", position=dodge, width=.75, aes(fill = vm),  )+
#   #   xlab("Benchmark") +
#   ylab("Relative Runtime") +
#   theme_bw(base_size=8, base_family="Helvetica") +
#   theme(
#     rect = element_rect(),
#     axis.title.x =  element_blank(),
#     #     axis.title.x = element_text(face="bold", size=9),
#     #     axis.text.x  = element_text(size=9), #angle=45, vjust=0.2,
#     axis.text.x  = element_text(size=8, angle=45, hjust=1),
#     axis.title.y = element_text(face="bold", size=8),
#     axis.text.y  = element_text(size=8), #angle=45, hjust=0.2, vjust=0.5,
#     legend.position=c(0.15, .75),
#     plot.margin = unit(c(-3.2,3,-4,-1),"mm"),
#     legend.text = element_text(size=7),
#     legend.title = element_text(size=7, face="bold"),
#     legend.background = element_rect(fill="gray90", size=0),
#     legend.margin = unit(0, "cm"),
#     legend.key=element_rect(fill="white"),
#     legend.key.size=unit(5,"mm")
#   ) +
#   scale_y_continuous(breaks=seq(0,ymax,.5), limits=c(0,ymax),expand=c(0,0)) +
#   scale_fill_brewer(name = "Virtual Machine", type="qual", palette="Set1") +
#   #facet_null()
#   facet_grid(. ~ overall, scales="free", space="free",labeller=label_bquote(""))
# if (rigorous) {
#   p <- p + geom_errorbar(aes(ymin=lower, ymax = upper),  position=dodge, color=I("black"), size=.33)  
# }
# p
# gg.file <- paste0(input.basename, "-norm-col.pdf")
# ggsave(gg.file, width=figure.width, height=figure.height, units=c("in"), colormodel='rgb')
# embed_fonts(gg.file, options=pdf.embed.options)
# 
# }

if ('ackermann' %in% bench$benchmark & 'cpstak' %in% bench$benchmark) {
  (function() {
    if (nrow(bench.summary.overall.all) <= 0) {
      return();
    }
    table.ltx.sel <- bench.summary.overall.all[c('vm','mean.norm')]
    table.ltx <- table.ltx.sel[2]
    rownames(table.ltx) <- gsub("^$", "Optimized", gsub("([A-Z])([A-Z][a-z])|([a-z0-9])([A-Z])","\\1\\3 \\2\\4", gsub("Pycket","", table.ltx.sel$vm)))
    colnames(table.ltx) <- c("\\llap{Relative Runtime (geometric mean)}")
    out <- latex(table.ltx,
                 file=paste0(input.basename, ".tex"),
                 rowlabel="",
                 #label="tbl:extremes",caption="Extreme runtimes",
                 booktabs=TRUE,
                 #ctable=TRUE,
                 cgroupTexCmd="mdseries",
                 rgroupTexCmd="mdseries",
                 table.env=FALSE, center="none",
                 rdec = c(0, rep(4, nrow(table.ltx) - 1)),
                 where="htbp", size="footnotesize", #center="centering",
)
  })()
  quit(safe="no")
}
if (rigorous) {
  # LaTeX table
  (function() {
    if (nrow(bench.summary.table.ltx) <= 0) {
      return();
    }
    len <- ncol(bench.summary.table.ltx)/2
    .just = rep(c('r','@{}>{\\smaller\\ensuremath{\\pm}}r@{\\,\\si{\\milli\\second}}'), len)
    .just = c('@{}r', .just[2:length(.just)])
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
                 colheads=rep(c('mean', ''),len),
                 col.just=.just,
  #                col.just=rep(c('r','@{\\scriptsize\\,\\ensuremath{\\pm}}>{\\scriptsize}r'), len),
                 cgroup=levels(as.factor(bench.summary.table$vm)),
                 cdec=rep(0, len*2))
  })()
  # LaTeX table, all
  (function() {
    if (nrow(bench.summary.ltx) <= 0) {
      return();
    }
    len <- ncol(bench.summary.ltx)/2
    .just = rep(c('r','@{}>{\\smaller\\ensuremath{\\pm}}r@{\\,\\si{\\milli\\second}}'), len)
    .just = c('@{}r', .just[2:length(.just)])
    .long <- nrow(bench.summary.ltx) > 50
    out <- latex(bench.summary.ltx,
                 file=paste0(input.basename, "-all.tex"),
                 rowlabel="Benchmark",
                 booktabs=TRUE,
                 table.env=(! .long), center="none",
                 longtable=.long,
                 size="small", #center="centering",
                 colheads=rep(c('mean', ''), len),
                 col.just=.just,
                 #col.just=rep(c('r','@{\\,\\si{\\milli\\second} \\ensuremath{\\pm}}r'), len),
                 cgroup=levels(as.factor(bench.summary$vm)),
                 cdec=rep(0, len*2))
  })()
  
} else {

  # LaTeX table
  (function() {
    if (nrow(bench.summary.table.ltx) <= 0) {
      return();
    }
    colnames(bench.summary.table.ltx) <- gsub(' mean', '', colnames(bench.summary.table.ltx))
    len <- ncol(bench.summary.table.ltx)
    .just = rep('@{}r', len)
    out <- latex(bench.summary.table.ltx,
                 file=paste0(input.basename, "-extremes.tex"),
                 rowlabel="",
                 booktabs=TRUE,
                 cgroupTexCmd="mdseries",
                 rgroupTexCmd="mdseries",
                 table.env=FALSE, center="none",
                 where="htbp", size="footnotesize", #center="centering",
                 col.just=.just,
                 cdec=rep(0, len))
  })()
  # LaTeX table, all
  (function() {
    if (nrow(bench.summary.ltx) <= 0) {
      return();
    }
    colnames(bench.summary.ltx) <- gsub(' mean', '', colnames(bench.summary.ltx))  
    len <- ncol(bench.summary.ltx)
    .just = rep('@{}r', len)
    .long <- nrow(bench.summary.ltx) > 50
    out <- latex(bench.summary.ltx,
                 file=paste0(input.basename, "-all.tex"),
                 rowlabel="Benchmark",
                 booktabs=TRUE,
                 table.env=(! .long), center="none",
                 longtable=.long,
                 size="small", #center="centering",
                 col.just=.just,
                 cdec=rep(0, len))
  })()
  
  
}

bench.info <- if ('fannkuch-redux' %in% bench$benchmark) {
  bench.summary.graph[bench.summary.graph$overall == FALSE,,] 
} else {
  bench.summary
}

if (multi.variate) {
  pycket.timings <- (bench.info[bench.info$variable_values == 'nothing' & bench.info$vm == 'Pycket',,])$mean.norm
} else {
  pycket.timings <- (bench.info[bench.info$vm == 'Pycket',,])$mean.norm
}
  
  
print(">> slowest pycket")
print(max(pycket.timings))
print(">> fastest pycket")
print(1/min(pycket.timings))

if (multi.variate) {
  bench.summary.nothing <- bench.info[bench.info$variable_values == 'nothing',,]
  winners <- bench.summary.nothing %>% group_by(benchmark) %>% filter(mean.norm == min(mean.norm))
} else {
  winners <- bench.info %>% group_by(benchmark) %>% filter(mean.norm == min(mean.norm))
}
winners <- winners[c('mean.norm','benchmark','vm')]
winners <- winners[do.call(order, winners),]
print("pycket wins in ");
print(winners[winners$vm == 'Pycket',,])
print("(this is")
print(nrow(winners[winners$vm == 'Pycket',,]))
print("out of")
print(nrow(winners))



print(">> done");