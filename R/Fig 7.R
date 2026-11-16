# Fig. 7 | Reproduce the reported PROTEST coefficients and check the source data.
# Keep Fig7.R, Fig7_data.xlsx and Original_global_permutations_99999.rds together.
# Rscript Fig7.R [--input=/path/Fig7_data.xlsx] [--full-permutations]
# Default: recompute all 11 coefficients and the first 499 null values, verify P counts, BH and GO selection.
# Full mode: repeat all 99,999 original permutations from the local RDS. The editable PPTX is the master artwork.

resolve_dir <- function() {
  for (f in rev(sys.frames())) if (!is.null(f$ofile) && file.exists(f$ofile)) return(dirname(normalizePath(f$ofile)))
  f<-sub('^--file=','',grep('^--file=',commandArgs(),value=TRUE))
  if(length(f)&&file.exists(f[1]))return(dirname(normalizePath(f[1])))
  stop('Run the saved R script or source its full path.')
}

check_fig7 <- function() {
  here<-resolve_dir();args<-commandArgs(TRUE)
  arg<-function(k,d){z<-args[startsWith(args,k)];if(length(z))substring(z[1],nchar(k)+1)else d}
  if(!requireNamespace('readxl',quietly=TRUE))stop('Missing R package: readxl')
  file<-normalizePath(arg('--input=',file.path(here,'Fig7_data.xlsx')),mustWork=TRUE)
  tab<-function(name)as.data.frame(readxl::read_excel(file,sheet=name,.name_repair='minimal',trim_ws=FALSE),check.names=FALSE)
  pots<-tab('匹配样本12盆');ids<-pots$PotID
  stopifnot(length(ids)==12L,!anyDuplicated(ids),all(table(pots$Treatment)==4L))
  coord<-tab('PROTEST与MFA坐标')
  get_matrix<-function(block){
    x<-coord[coord$Block==block,];n<-max(x$Axis_index)
    stopifnot(setequal(x$PotID,ids),nrow(x)==12L*n,!anyDuplicated(x[c('PotID','Axis_index')]),all(is.finite(x$Coordinate)))
    out<-matrix(NA_real_,12L,n,dimnames=list(ids,NULL));out[cbind(match(x$PotID,ids),x$Axis_index)]<-x$Coordinate
    stopifnot(!anyNA(out));out
  }
  centered<-function(x)sweep(x,2,colMeans(x),'-')
  normal<-function(x){x<-centered(x);x/sqrt(sum(x^2))}
  stat<-function(x,y)sum(svd(crossprod(x,y),nu=0L,nv=0L)$d)
  perm<-as.matrix(tab('置换前499索引'));null<-tab('置换前499统计量')
  stopifnot(identical(dim(perm),c(499L,12L)),all(apply(perm,1,function(x)identical(sort(as.integer(x)),1:12))))
  edges<-tab('PROTEST连线结果');pool<-tab('BH校正181项')
  stopifnot(nrow(edges)==11L,sum(edges$Display)==10L,nrow(pool)==181L,!anyDuplicated(pool$Key),all(edges$N==12L))
  q<-p.adjust(pool$P,method='BH');ix<-match(edges$Key,pool$Key)
  stopifnot(!anyNA(ix),max(abs(q-pool$BH_q))<1e-12,max(abs(q[ix]-edges$BH_q))<1e-12,
            max(abs(pool$P[ix]-edges$P))<1e-12)
  stars<-ifelse(edges$BH_q<.001,'***',ifelse(edges$BH_q<.01,'**',ifelse(edges$BH_q<.05,'*','ns')))
  stopifnot(identical(stars,edges$Stars),identical(paste0(sprintf('%.3f',edges$R),ifelse(stars=='ns',' ns',stars)),edges$Label))

  full<-NULL
  if('--full-permutations'%in%args){
    index_file<-file.path(here,'Original_global_permutations_99999.rds')
    if(!file.exists(index_file))stop('Missing original permutation index: ',index_file)
    full<-readRDS(index_file)
    stopifnot(identical(dim(full),c(99999L,12L)),all(full[1:499,]==perm))
  }
  ans<-lapply(seq_len(nrow(edges)),function(i){
    z<-edges[i,];x<-normal(get_matrix(z$A));y<-normal(get_matrix(z$B));r<-stat(x,y)
    rows<-null[null$Edge==z$Edge,];rows<-rows[order(rows$Permutation),]
    stopifnot(nrow(rows)==499L,all(rows$Permutation==1:499))
    values<-vapply(seq_len(nrow(perm)),function(j)stat(x,y[perm[j,],,drop=FALSE]),numeric(1))
    stopifnot(abs(r-z$R)<1e-10,max(abs(values-rows$R))<1e-10,
              abs((z$Null_exceedances+1)/(z$Permutations+1)-z$P)<1e-12)
    if(!is.null(full)){
      count<-sum(vapply(seq_len(nrow(full)),function(j)stat(x,y[full[j,],,drop=FALSE])>=r-sqrt(.Machine$double.eps),logical(1)))
      if(count!=z$Null_exceedances)stop('Full permutation count differs for ',z$Edge)
    }
    data.frame(Edge=z$Edge,Display=z$Display,R=r,P=z$P,BH_q=z$BH_q,Label=z$Label)
  })
  soil<-tab('土壤四指标');soil<-soil[match(ids,soil$PotID),]
  soil_z<-scale(as.matrix(soil[,c('Soil_AN_mg_kg','Soil_UE','Soil_DOC_mg_kg','Soil_ACP')]))
  stopifnot(max(abs(as.matrix(dist(soil_z))-as.matrix(dist(get_matrix('AICcSoil4')))))<1e-10)
  for(target in c('Microbiome_MFA','Transcriptome_MFA')){
    names<-if(target=='Microbiome_MFA')c('Original_Genus','Original_KO') else c('Original_RootRNA','Original_ShootRNA')
    blocks<-lapply(names,function(name){x<-centered(get_matrix(name));x/max(svd(x,nu=0L,nv=0L)$d)})
    combined<-do.call(cbind,blocks);expected<-get_matrix(target)
    stopifnot(max(abs(tcrossprod(combined)-tcrossprod(expected)))<1e-10)
  }
  allgo<-tab('GO候选全表');shown<-tab('GO绘图条目')
  for(organ in c('Root','Shoot'))for(sign in c(1,-1)){
    keep<-which(allgo$Organ==organ&allgo$BH_adjusted_P<.05&sign*allgo$NES>0)
    x<-allgo[keep[order(-abs(allgo$NES[keep]),method='radix')],]
    x<-head(x[!duplicated(x$GO),],2);y<-shown[which(shown$Organ==organ&sign*shown$NES>0),]
    y<-y[match(x$GO,y$GO),];stopifnot(nrow(x)==2L,nrow(y)==2L,!anyNA(y$GO),
      identical(x$Analysis,y$Analysis),max(abs(x$NES-y$NES))<1e-12)
  }
  print(do.call(rbind,ans),row.names=FALSE)
  message('Verified 11 PROTEST coefficients, BH across 181 tests, MFA coordinates and eight GO labels.')
  message(if(is.null(full))'Verified the first 499 null values per link and the original full-test P counts.' else 'Reproduced all 99,999 original permutations per link.')
  invisible(TRUE)
}
check_fig7()
