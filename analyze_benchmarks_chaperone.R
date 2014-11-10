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
  tsv_name <- "output/current-chap.tsv"
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

bench$vm <- factor(bench$vm, levels = c("Pycket", "Racket", "Larceny", "Gambit", "Bigloo", "V8", "Spidermonkey", "Python", "Pypy"))
bench$suite <- factor(gsub("Chaperone(\\w+)Benchmarks", "\\1", bench$suite))
reference.vm <-  if ('Racket' %in% bench$vm) 'Racket' else 'Pycket'



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


bench <- bench[c('criterion','vm','suite','benchmark','value')]



rigorous <- 1 < nrow(bench[
  bench$benchmark == bench$benchmark[[1]] &
  bench$vm == bench$vm[[1]] &
  bench$suite == bench$suite[[1]] &
  bench$criterion == bench$criterion[[1]]
,])


bench.corr = data.frame()
for (suite in levels(bench$suite)) {
  .s <- bench[bench$suite == suite,, ]
  for (crit in levels(droplevels(bench$criterion))) {
    for (vm in levels(droplevels(bench$vm))) {  
      for (benchmark in levels(droplevels(.s$benchmark))) {
        if (1 > nrow(.s[(
            bench$criterion == crit & 
            bench$vm == vm &
            bench$benchmark == benchmark
          ),,])) {
          .x <- data.frame(criterion=crit,vm=vm,suite=suite,benchmark=benchmark,value=NA)
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


######################################################

bench.tot <- droplevels(bench[bench$criterion == 'total',,drop=TRUE])
bench.cpu <- droplevels(bench[bench$criterion == "cpu",,drop=TRUE])

group.by <- as.quoted(c("suite", "benchmark","vm"))

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
} else {
  bench.summary <- ddply(bench.tot, group.by, plyr::summarize, 
                         overall=FALSE,
                         mean=mean(value),
                         median=median(value)
  )
}



sel.col = if (rigorous) { c('suite','benchmark','vm','mean','err095') } else 
                        { c('suite','benchmark','vm','mean') }
bench.summary.sel <- dcast(melt(bench.summary[sel.col], id.vars=c('suite', 'benchmark','vm')), suite + benchmark ~ vm + variable)
bench.summary.ltx <- bench.summary.sel[3:length(bench.summary.sel)]
rownames(bench.summary.ltx) <- paste0(bench.summary.sel$suite, '\\textsubscript{', bench.summary.sel$benchmark, '}')
colnames(bench.summary.ltx) <- sapply(colnames(bench.summary.ltx), function(x) {sedit(x, '_', ' ')})

# ----- Outputting -----

# Excel for overall data
write.xlsx(bench.tot, paste0(input.basename, ".xlsx"), append=FALSE, sheetName="bench")
write.xlsx(bench.summary, paste0(input.basename, ".xlsx"), append=TRUE, sheetName="summary")

# 
if (rigorous) {
  # LaTeX table, all
  (function() {
    if (nrow(bench.summary.ltx) <= 0) {
      return();
    }
    len <- ncol(bench.summary.ltx)/2
    .just = rep(c('r','@{}>{\\smaller\\ensuremath{\\pm}}r@{\\,\\si{\\milli\\second}}'), len)
    .just = c('@{}r', .just[2:length(.just)])
    .long <- nrow(bench.summary.ltx) > 50
    .side <- (!.long & len > 5)
    out <- latex(bench.summary.ltx,
                 file=paste0(input.basename, "-all.tex"),
                 rowlabel="Benchmark",
                 booktabs=TRUE,
                 table.env=(! .long), center="none",
                 longtable=.long,
                 landscape=.side,
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