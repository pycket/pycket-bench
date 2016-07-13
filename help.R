#! /usr/bin/env Rscript

use <- function(pkg) {
  if (!require(pkg, character.only=TRUE)) { install.packages(pkg, repos="http://cran.rstudio.com") }
  library(pkg,character.only=TRUE)
}

sapply(pkgs, use)

if (!require(devtools)) {
  install.packages("devtools", repos="http://cran.rstudio.com")
  library(devtools)
}

if (!require(extrafont)) {
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

if (cmd.line) {
  if (length(commandArgs(trailingOnly=TRUE)) > 0) {
    tsv_name = commandArgs(trailingOnly=TRUE)[1]
  } else {
    tsv_name <- tsv_name.default  
  }
  
  if (!file.exists(tsv_name)) {
    stop("Cannot open input file ", tsv_name)
  }
} else {
  tsv_name <- tsv_name.default  
}  
  
input.basename = file_path_sans_ext(tsv_name)

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

latexify <- function(df.sel, var){
  df.ltx <- df.sel[2:length(df.sel)]
  rownames(df.ltx) <- df.sel[[var]]
  colnames(df.ltx) <- sapply(colnames(df.ltx), function(x) {sedit(x, '_', ' ')})
  df.ltx
}

fill.missing <- function(bench) {
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
 rbind(bench, bench.corr)
}

summarize.bench <- function(bench, rigorous, multi.variate, group.by, reference.vm) {
  if (rigorous) {
    bench.summary <- ddply(bench, group.by, plyr::summarize, 
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
        .tot <- droplevels(bench[bench$variable_values == var.val,,drop=TRUE])
        .err <- bootstrapTo(.tot, 'benchmark', 'vm', reference.vm, 'value')
        .norm <- normalizeTo(.sum, 'benchmark', 'vm', reference.vm, 'mean')
        .merge <- merge(.norm, .err)
        df <- rbind(df, .merge)
      }
      bench.summary <- df
    } else {
      bench.err <- bootstrapTo(bench, 'benchmark', 'vm', reference.vm, 'value')
      bench.summary <- merge(
        normalizeTo(bench.summary, 'benchmark', 'vm', reference.vm, 'mean'),
        bench.err
      )
    }
  } else {
    bench.summary <- ddply(bench, group.by, plyr::summarize, 
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
  bench.summary
}

default.theme <- function() {
    theme_bw(base_size=6, base_family="Helvetica") +
    theme(
      rect = element_rect(),
      axis.title.x =  element_blank(),
      axis.text.x  = element_text(size=6, angle=45, hjust=1),
      axis.title.y = element_text(face="bold", size=6),
      axis.text.y  = element_text(size=6), #angle=45, hjust=0.2, vjust=0.5,
      legend.position=c(0.15, .8),
      plot.margin = unit(c(-3.2,3,-3,-0.5),"mm"),
      legend.text = element_text(size=6),
      legend.title = element_text(size=6, face="bold"),
      legend.background = element_rect(fill="gray90", size=0),
      legend.margin = unit(0, "cm"),
      legend.key=element_rect(fill="white"),
      legend.key.size=unit(3,"mm")
    )
}

overall.labeller <- function(var, value) {
  if (var == 'overall') {
    label_bquote("")(var, value)
  } else {
    label_value(var, value)
  }  
}
