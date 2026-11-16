# One R entry, one multi-sheet workbook, and the accepted TIFF/SVG figure.
# Run this saved file with Rscript. --output=... writes to another folder.
# --no-save-data reproduces figures without refreshing the workbook.
resolve_dir<-function(){
 for(fr in rev(sys.frames()))if(!is.null(fr$ofile)&&file.exists(fr$ofile))return(dirname(normalizePath(fr$ofile)))
 ff<-sub('^--file=','',grep('^--file=',base::commandArgs(),value=TRUE))
 if(length(ff)&&file.exists(ff[1]))return(dirname(normalizePath(ff[1])))
 if(requireNamespace('rstudioapi',quietly=TRUE)&&rstudioapi::isAvailable()){
  ff<-rstudioapi::getActiveDocumentContext()$path;if(nzchar(ff)&&file.exists(ff))return(dirname(normalizePath(ff)))
 }
 stop('Run the saved R file using Rscript or source its full path.')
}
# Refresh values while preserving the workbook layout and embedded original photograph.
refresh_workbook<-function(sheets,destination,source){
 olddir<-tempfile('fig2_book_source_');newdir<-tempfile('fig2_book_output_')
 dir.create(olddir);dir.create(newdir)
 on.exit(unlink(c(olddir,newdir),recursive=TRUE),add=TRUE)
 initial<-tempfile(fileext='.xlsx');on.exit(unlink(initial),add=TRUE)
 writexl::write_xlsx(sheets,initial)
 utils::unzip(initial,exdir=newdir);utils::unzip(source,exdir=olddir)
 for(i in seq_along(sheets)){
  rel<-file.path('xl','worksheets',paste0('sheet',i,'.xml'))
  template<-xml2::read_xml(file.path(olddir,rel));updated<-xml2::read_xml(file.path(newdir,rel))
  oldcells<-xml2::xml_find_all(template,"//*[local-name()='sheetData']//*[local-name()='c']")
  style<-setNames(xml2::xml_attr(oldcells,'s'),xml2::xml_attr(oldcells,'r'))
  newcells<-xml2::xml_find_all(updated,"//*[local-name()='sheetData']//*[local-name()='c']")
  addresses<-xml2::xml_attr(newcells,'r');styles<-unname(style[addresses])
  for(j in seq_along(styles))if(is.na(styles[j])){
   column<-sub('[0-9]+$','',addresses[j]);row<-if(sub('^[A-Z]+','',addresses[j])=='1')'1'else'2'
   styles[j]<-style[paste0(column,row)]
  }
  styles[is.na(styles)]<-'0';xml2::xml_set_attr(newcells,'s',styles)
  rows<-xml2::xml_find_all(updated,"//*[local-name()='sheetData']/*[local-name()='row']")
  xml2::xml_set_attr(rows,'ht',ifelse(xml2::xml_attr(rows,'r')=='1','42','20'))
  xml2::xml_set_attr(rows,'customHeight','1')
  xml2::xml_replace(xml2::xml_find_first(template,"//*[local-name()='sheetData']"),xml2::xml_find_first(updated,"//*[local-name()='sheetData']"))
  xml2::write_xml(template,file.path(newdir,rel))
 }
 if(!file.copy(file.path(olddir,'xl','styles.xml'),file.path(newdir,'xl','styles.xml'),overwrite=TRUE))stop('Could not preserve workbook formatting.')
 for(folder in c('drawings','media','theme'))if(dir.exists(file.path(olddir,'xl',folder))){
  if(!file.copy(file.path(olddir,'xl',folder),file.path(newdir,'xl'),recursive=TRUE))stop('Could not preserve workbook images or theme.')
 }
 oldrels<-file.path(olddir,'xl','worksheets','_rels')
 if(dir.exists(oldrels)&&!file.copy(oldrels,file.path(newdir,'xl','worksheets'),recursive=TRUE))stop('Could not preserve image relationships.')
 oldct<-xml2::read_xml(file.path(olddir,'[Content_Types].xml'));newct<-xml2::read_xml(file.path(newdir,'[Content_Types].xml'))
 for(node in xml2::xml_children(xml2::xml_root(oldct))){
  part<-xml2::xml_attr(node,'PartName');ext<-xml2::xml_attr(node,'Extension')
  needed<-(!is.na(part)&&(startsWith(part,'/xl/drawings/')||startsWith(part,'/xl/theme/')))||(!is.na(ext)&&ext%in%c('png','jpg','jpeg'))
  existing<-xml2::xml_children(xml2::xml_root(newct))
  already<-if(!is.na(part))part%in%xml2::xml_attr(existing,'PartName')else ext%in%xml2::xml_attr(existing,'Extension')
  if(needed&&!already)xml2::xml_add_child(xml2::xml_root(newct),node)
 }
 xml2::write_xml(newct,file.path(newdir,'[Content_Types].xml'))
 tmp<-tempfile('fig2_book_',tmpdir=dirname(destination),fileext='.xlsx');on.exit(unlink(tmp),add=TRUE)
 zip::zipr(tmp,files=list.files(newdir,recursive=TRUE,all.files=TRUE,include.dirs=FALSE),root=newdir,include_directories=FALSE,mode='mirror')
 if(!file.rename(tmp,destination))stop('Could not replace the data workbook. Close it in Excel and retry.')
}

run_figure<-function(){
 script_dir<-resolve_dir();args<-base::commandArgs(trailingOnly=TRUE)
 arg<-function(k,d){x<-args[startsWith(args,k)];if(length(x))substring(x[1],nchar(k)+1)else d}
 input_file<-arg('--input=',file.path(script_dir,paste0(r"----(FigS6)----",'_data.xlsx')))
 output_root<-arg('--output=',script_dir);dir.create(output_root,recursive=TRUE,showWarnings=FALSE)
 if(!file.exists(input_file))stop('Missing workbook: ',input_file)
 input_file<-normalizePath(input_file);initial_hash<-tools::md5sum(input_file)
 pkgs<-c('readxl','jsonlite','writexl','xml2','zip')
 missing<-pkgs[!vapply(pkgs,requireNamespace,quietly=TRUE,FUN.VALUE=logical(1))]
 if(length(missing))stop('Missing R packages: ',paste(missing,collapse=', '))
 specs<-jsonlite::fromJSON(r"----([{"name":"原始数据","columns":["Treatment","Rep","order","SDW","RDW","SCd","RCd","TF","SBCF","RBCF","ECd","RN","RTN","L","AD","SA","V","SPAD","Ci","gs","A","E","WUE"],"kinds":["string","number","number","number","number","number","number","number","number","number","number","number","number","number","number","number","number","number","number","number","number","number","number"],"target":"data/公共数据/筛选实测数据.CSV"},{"name":"运算数据","columns":["Treatment","Rep","order","SDW","RDW","SCd","RCd","TF","SBCF","RBCF","ECd","RN","RTN","L","AD","SA","V","SPAD","Ci","gs","A","E","WUE"],"kinds":["string","number","number","number","number","number","number","number","number","number","number","number","number","number","number","number","number","number","number","number","number","number","number"],"target":"data/公共数据/analysis_data_with_calculations.csv"},{"name":"Spearman完整结果","columns":["i","j","var1","var2","rho","P_raw","n","q_BH_66","stars","label"],"kinds":["number","number","string","string","number","number","number","number","string","number"],"target":"data/FigS6/results/FigS6_Spearman_complete.csv"},{"name":"处理均值","columns":["Treatment","SDW","RDW","SCd","RCd","TF","ECd","RN","L","SA","SPAD","A","WUE"],"kinds":["string","number","number","number","number","number","number","number","number","number","number","number","number"],"target":"data/FigS6/results/FigS6_treatment_means.csv"},{"name":"Panel对应表","columns":["Figure","Panel","Content","Data_path","Variables","Observation_count","Statistics_path","Workbook_data_sheet","Workbook_statistics_sheet"],"kinds":["string","string","string","string","string","string","string","string","string"],"target":"data/Panel_Data_Index.csv"}])----",simplifyVector=FALSE)
 tables<-setNames(lapply(specs,function(s){
  types<-vapply(s$kinds,function(k)switch(k,number='numeric',boolean='logical',string='text'),character(1))
  d<-as.data.frame(readxl::read_excel(input_file,sheet=s$name,col_types=types,.name_repair='minimal',na=character()),check.names=FALSE)
  stopifnot(identical(names(d),unlist(s$columns)))
  for(j in seq_along(types))if(types[j]=='text')d[[j]][is.na(d[[j]])]<-''
  d
 }),vapply(specs,`[[`,character(1),'name'))
 stage<-tempfile(paste0(r"----(FigS6)----",'_'));dir.create(stage);on.exit(unlink(stage,recursive=TRUE),add=TRUE)
 for(s in specs)if(!is.null(s$target)){
  p<-file.path(stage,s$target);dir.create(dirname(p),recursive=TRUE,showWarnings=FALSE)
  if(grepl('[.]json$',p))writeLines(tables[[s$name]]$JSON[1],p,useBytes=TRUE)else write.csv(tables[[s$name]],p,row.names=FALSE,na='NA')
 }
 asset_specs<-jsonlite::fromJSON(r"----([])----",simplifyVector=FALSE)
 if(length(asset_specs)){
  media<-utils::unzip(input_file,list=TRUE)$Name;media<-media[grepl('^xl/media/',media)&!grepl('/$',media)]
  extract<-file.path(stage,'_images');dir.create(extract);utils::unzip(input_file,files=media,exdir=extract)
  images<-file.path(extract,media);hashes<-unname(tools::md5sum(images))
  for(asset in asset_specs){
   hit<-which(hashes==asset$md5);if(length(hit)!=1)stop('The original image is missing or changed: ',asset$sheet)
   p<-file.path(stage,asset$target);dir.create(dirname(p),recursive=TRUE,showWarnings=FALSE);stopifnot(file.copy(images[hit],p))
  }
 }
 
 code_files<-list(r"----(code/figS2_S6.R)----"=r"----(# Supplementary Figures S2-S6: unchanged statistics and TIFF/SVG exports.
# Run the saved script with Rscript; optional --input=... and --output=...
args<-commandArgs(trailingOnly=TRUE)
resolve_code_dir<-function(){
 for(fr in rev(sys.frames()))if(!is.null(fr$ofile)&&file.exists(fr$ofile))return(dirname(normalizePath(fr$ofile)))
 ff<-sub('^--file=','',grep('^--file=',commandArgs(),value=TRUE))
 if(length(ff)&&file.exists(ff[1]))return(dirname(normalizePath(ff[1])))
 if(requireNamespace('rstudioapi',quietly=TRUE)&&rstudioapi::isAvailable()){
  ff<-rstudioapi::getActiveDocumentContext()$path
  if(nzchar(ff)&&file.exists(ff))return(dirname(normalizePath(ff)))
 }
 stop('Run the saved script using Rscript or source its full path.')
}
fig_dir<-dirname(resolve_code_dir())
arg<-function(k,d){z<-args[startsWith(args,k)];if(length(z))substring(z[1],nchar(k)+1)else d}
input<-arg('--input=',file.path(fig_dir,'data','公共数据','筛选实测数据.CSV'))
out<-arg('--output=',fig_dir);data_out<-file.path(out,'data');common_out<-file.path(data_out,'公共数据')
invisible(lapply(c(file.path(out,'figure'),common_out),dir.create,showWarnings=FALSE,recursive=TRUE))
figure_result<-function(name,suffix){d<-file.path(data_out,name,'results');dir.create(d,recursive=TRUE,showWarnings=FALSE);file.path(d,paste0(name,suffix))}
pkgs<-c('ggplot2','dplyr','tidyr','rstatix','car','multcompView','patchwork','ragg','svglite','magick')
stopifnot(all(vapply(pkgs,requireNamespace,quietly=TRUE,FUN.VALUE=logical(1))))
suppressPackageStartupMessages({library(ggplot2);library(dplyr);library(tidyr);library(patchwork);library(grid)})
font<-'Arial';ink<-'#263238';before<-tools::md5sum(input)
heatmap_choice<-arg('--heatmap-palette=','A')
heat_schemes<-list(
 A=list(root=c('#32979C','#E7E3DB','#D9984C'),photo=c('#32979C','#E7E3DB','#D9984C')),
 B=list(root=c('#5D8FBE','#E6E1D8','#C99F42'),photo=c('#5D8FBE','#E6E1D8','#C99F42')),
 C=list(root=c('#8A9C57','#E7E5DC','#9B7FB8'),photo=c('#8A9C57','#E7E5DC','#9B7FB8')))
stopifnot(heatmap_choice%in%names(heat_schemes))

raw<-read.csv(input,fileEncoding='UTF-8-BOM',check.names=FALSE)
needed<-c('SDW','RDW','SCd','RCd','TF','SBCF','RBCF','ECd','RN','RTN','L','AD','SA','V','SPAD','Ci','gs','A','E','WUE')
stopifnot(nrow(raw)==114L,length(unique(raw$Treatment))==19L,all(table(raw$Treatment)==6L),
 all(vapply(raw[needed],function(x)is.numeric(x)&&all(is.finite(x)),logical(1))),
 !anyDuplicated(paste(raw$Treatment,raw$Rep)))
levels_trt<-as.character(raw$Treatment[order(raw$order)])[!duplicated(raw$Treatment[order(raw$order)])]
stopifnot(levels_trt[1]=='CK')
dat<-raw;dat$Treatment<-factor(dat$Treatment,levels=levels_trt)
# Exact calculation is used in working data; measured source columns are untouched.
dat$TF<-dat$SCd/dat$RCd;dat$SBCF<-dat$SCd/3.46;dat$RBCF<-dat$RCd/3.46
dat$ECd<-dat$SDW*dat$SCd;dat$WUE<-dat$A/dat$E
write.csv(dat,file.path(common_out,'analysis_data_with_calculations.csv'),row.names=FALSE)
# Preserve the previously used decision rule; do not tune tests to plotted results.
test_one<-function(v){
 d<-data.frame(Treatment=dat$Treatment,y=dat[[v]])
 fit<-lm(y~Treatment,d);sp<-shapiro.test(residuals(fit))$p.value
 lp<-car::leveneTest(y~Treatment,d,center=median)$'Pr(>F)'[1]
 if(sp>.05 && lp>.05){
  a<-aov(y~Treatment,d);tt<-TukeyHSD(a)$Treatment
  groups<-strsplit(rownames(tt),'-',fixed=TRUE)
  pw<-data.frame(group1=vapply(groups,'[',character(1),1),group2=vapply(groups,'[',character(1),2),P_adjusted=tt[,'p adj'])
  method<-'ANOVA + Tukey HSD';op<-summary(a)[[1]]$'Pr(>F)'[1]
 }else if(sp>.05){
  z<-as.data.frame(rstatix::games_howell_test(d,y~Treatment,detailed=TRUE))
  pw<-data.frame(group1=z$group1,group2=z$group2,P_adjusted=z$p.adj)
  method<-'Welch ANOVA + Games–Howell';op<-oneway.test(y~Treatment,d,var.equal=FALSE)$p.value
 }else{
  z<-as.data.frame(rstatix::dunn_test(d,y~Treatment,p.adjust.method='BH',detailed=TRUE))
  pw<-data.frame(group1=z$group1,group2=z$group2,P_raw=z$p,P_adjusted=z$p.adj)
  method<-'Kruskal–Wallis + Dunn (BH)';op<-kruskal.test(y~Treatment,d)$p.value
 }
 if(!'P_raw'%in%names(pw))pw$P_raw<-NA_real_
 means<-tapply(d$y,d$Treatment,mean);ord<-names(sort(means,decreasing=TRUE))
 pm<-matrix(1,19,19,dimnames=list(ord,ord))
 for(i in seq_len(nrow(pw))){pm[pw$group1[i],pw$group2[i]]<-pm[pw$group2[i],pw$group1[i]]<-pw$P_adjusted[i]}
 labs<-multcompView::multcompLetters(pm)$Letters
 for(i in seq_len(nrow(pw))){
  shared<-length(intersect(strsplit(labs[pw$group1[i]],'')[[1]],strsplit(labs[pw$group2[i]],'')[[1]]))>0
  stopifnot(shared==(pw$P_adjusted[i]>=.05))
 }
 sm<-do.call(rbind,lapply(levels_trt,function(g){x<-d$y[d$Treatment==g];data.frame(
 Variable=v,Treatment=g,n=length(x),mean=mean(x),SD=sd(x),SE=sd(x)/sqrt(length(x)),
 median=median(x),Q1=unname(quantile(x,.25)),Q3=unname(quantile(x,.75)),minimum=min(x),maximum=max(x),Letters=unname(labs[g]))}))
 list(summary=sm,pairs=transform(pw,Variable=v,Method=method),
 method=data.frame(Variable=v,Method=method,Shapiro_residual_P=sp,Levene_median_P=lp,Overall_P=op,n=114,groups=19))
}
tests<-setNames(lapply(needed,test_one),needed)
summaries<-bind_rows(lapply(tests,'[[','summary'))
pairs<-bind_rows(lapply(tests,'[[','pairs'));methods<-bind_rows(lapply(tests,'[[','method'))
names_full<-c(SDW='Shoot dry weight',RDW='Root dry weight',SCd='Shoot Cd concentration',RCd='Root Cd concentration',
 TF='Translocation factor',ECd='Shoot Cd extraction',RN='Root number',RTN='Root tip number',L='Root length',
 AD='Average root diameter',SA='Root surface area',V='Root volume',SPAD='Leaf chlorophyll index (SPAD)',
 Ci='Intercellular CO₂ concentration',gs='Stomatal conductance',A='Net photosynthetic rate',E='Transpiration rate',WUE='Water use efficiency')
common<-theme_classic(base_family=font,base_size=7.5)+theme(
 text=element_text(colour=ink),axis.title=element_text(size=8.5),axis.text=element_text(size=7.5,colour=ink),
 axis.line=element_line(linewidth=.3),axis.ticks=element_line(linewidth=.3),axis.ticks.length=unit(1,'mm'),
 axis.title.y=element_text(margin=margin(r=4)),legend.title=element_text(size=8.5),legend.text=element_text(size=7.5),
 legend.position='right',plot.margin=margin(3,4,3,3,'mm'),plot.tag=element_text(size=11,face='bold',family=font),
 plot.background=element_rect(fill='white',colour=NA))
plot_trait<-function(v,ylab,tag){
 d<-data.frame(Treatment=dat$Treatment,y=dat[[v]])
 d$x<-as.integer(d$Treatment)+ave(seq_len(nrow(d)),d$Treatment,FUN=function(i)seq(-.14,.14,length.out=length(i)))
 sm<-tests[[v]]$summary;sm$x<-match(sm$Treatment,levels_trt)
 spread<-diff(range(d$y));sm$ly<-pmax(sm$maximum,sm$mean+sm$SE)+.065*spread
 sm$ly<-sm$ly+rep(c(0,.018),length.out=nrow(sm))*spread
 p<-ggplot(d,aes(as.numeric(Treatment),y))
 if(v!='ECd')p<-p+geom_boxplot(aes(group=Treatment),width=.60,fill=if(v%in%c('SDW','RDW'))'#86A6BC'else'#69AE9B',colour=if(v%in%c('SDW','RDW'))'#48677D'else'#316B5B',linewidth=.35,outlier.shape=NA)
 else p<-p+geom_errorbar(data=sm,aes(x=x,ymin=mean-SE,ymax=mean+SE),inherit.aes=FALSE,width=.20,linewidth=.4)+
  geom_point(data=sm,aes(x=x,y=mean),inherit.aes=FALSE,shape=23,fill='#C58945',size=2.0,stroke=.4)
 p+geom_point(aes(x=x),shape=21,size=1.05,stroke=.25,fill='white',colour='#3E4C55')+
  geom_text(data=sm,aes(x=x,y=ly,label=Letters),inherit.aes=FALSE,size=7/.pt,family=font,vjust=0)+
  scale_x_continuous(breaks=1:19,labels=levels_trt,limits=c(.4,19.6),expand=c(0,0))+
  scale_y_continuous(expand=expansion(mult=c(if(v%in%c('SDW','RDW','ECd'))0 else .06,.13)),n.breaks=4)+
  labs(x=NULL,y=ylab,tag=tag)+common+
  theme(plot.margin=margin(.5,2,.5,1.5,'mm'),axis.text.x=element_text(angle=45,hjust=1,vjust=1),panel.border=element_rect(fill=NA,linewidth=.3),axis.line=element_blank())
}
s3<-wrap_plots(
 plot_trait('SCd',expression(atop('Shoot Cd concentration','(mg '*kg^{-1}*')')),'a'),
 plot_trait('RCd',expression(atop('Root Cd concentration','(mg '*kg^{-1}*')')),'b'),
 plot_trait('TF','Translocation\nfactor','c'),
 plot_trait('ECd',expression(atop('Shoot Cd extraction','(' *mu*'g '*plant^{-1}*')')),'d'),
 ncol=1)
make_heat<-function(vars,name){
 means<-do.call(rbind,lapply(levels_trt,function(g)data.frame(Treatment=g,as.list(colMeans(dat[dat$Treatment==g,vars,drop=FALSE])),check.names=FALSE)))
 z<-scale(as.matrix(means[,vars]));colnames(z)<-vars;rownames(z)<-levels_trt
 long<-do.call(rbind,lapply(vars,function(v)data.frame(Treatment=levels_trt,Variable=v,Z=z[,v])))
 long<-merge(long,summaries[c('Variable','Treatment','Letters')],by=c('Variable','Treatment'),sort=FALSE)
 long$x<-match(long$Treatment,levels_trt);long$y<-match(long$Variable,vars)
 lim<-ceiling(max(abs(long$Z))*2)/2
 pal<-if(name=='FigS4')heat_schemes[[heatmap_choice]]$root else heat_schemes[[heatmap_choice]]$photo
 write.csv(means,figure_result(name,'_treatment_means.csv'),row.names=FALSE)
 write.csv(long,figure_result(name,'_heatmap_values.csv'),row.names=FALSE)
 ggplot(long,aes(x,y,fill=Z))+geom_tile(width=.99,height=.99,colour='white',linewidth=.2)+
 geom_text(aes(label=Letters),family=font,size=6.8/.pt,colour='#111111')+
 scale_fill_gradient2(low=pal[1],mid=pal[2],high=pal[3],midpoint=0,limits=c(-lim,lim),name='Z-score')+
 scale_x_continuous(breaks=1:19,labels=levels_trt,limits=c(.5,19.5),expand=c(0,0))+
 scale_y_reverse(breaks=seq_along(vars),labels=unname(names_full[vars]),limits=c(length(vars)+.5,.5),expand=c(0,0))+
 labs(x=NULL,y=NULL)+common+theme(panel.grid=element_blank(),axis.line=element_blank(),axis.ticks=element_blank(),
 axis.text.x=element_text(angle=45,hjust=1,vjust=1),legend.key.height=unit(6,'mm'),
 plot.margin=margin(3,4,3,3,'mm'))
}
s4<-make_heat(c('RN','RTN','L','AD','SA','V'),'FigS4')
s5<-make_heat(c('SPAD','Ci','gs','A','E','WUE'),'FigS5')
# S6 retains the manuscript's descriptive, treatment-mean analysis (n=19).
vars6<-c('SDW','RDW','SCd','RCd','TF','ECd','RN','L','SA','SPAD','A','WUE')
means6<-do.call(rbind,lapply(levels_trt,function(g)data.frame(Treatment=g,as.list(colMeans(dat[dat$Treatment==g,vars6,drop=FALSE])),check.names=FALSE)))
co<-do.call(rbind,lapply(1:12,function(i)do.call(rbind,lapply(seq_len(i),function(j){
 if(i==j)return(data.frame(i=i,j=j,var1=vars6[i],var2=vars6[j],rho=1,P_raw=NA_real_,n=19))
 t<-cor.test(means6[[vars6[i]]],means6[[vars6[j]]],method='spearman',exact=FALSE)
 data.frame(i=i,j=j,var1=vars6[i],var2=vars6[j],rho=unname(t$estimate),P_raw=t$p.value,n=19)
}))))
co$q_BH_66<-NA_real_;ix<-co$i!=co$j;co$q_BH_66[ix]<-p.adjust(co$P_raw[ix],'BH')
co$stars<-ifelse(is.na(co$P_raw),'',ifelse(co$P_raw<.001,'***',ifelse(co$P_raw<.01,'**',ifelse(co$P_raw<.05,'*',''))))
co$label<-ifelse(co$i==co$j,'1.00',sprintf('%.2f',co$rho))
write.csv(means6,figure_result('FigS6','_treatment_means.csv'),row.names=FALSE)
write.csv(co,figure_result('FigS6','_Spearman_complete.csv'),row.names=FALSE)
s6<-ggplot(co,aes(j,i,fill=rho))+geom_tile(colour='white',linewidth=.25)+
 geom_text(aes(label=label),nudge_y=-.10,family=font,size=7.2/.pt)+
 geom_text(data=co[co$stars!='',],aes(label=stars),nudge_y=.20,family=font,size=6.4/.pt)+
 scale_fill_gradient2(low='#4393C3',mid='#F7F7F7',high='#D6604D',limits=c(-1,1),name='Spearman ρ')+
 scale_x_continuous(breaks=1:12,labels=unname(names_full[vars6]),expand=c(0,0),limits=c(.5,12.5))+
 scale_y_reverse(breaks=1:12,labels=unname(names_full[vars6]),expand=c(0,0),limits=c(12.5,.5))+
 coord_fixed()+labs(x=NULL,y=NULL)+common+theme(panel.grid=element_blank(),axis.line=element_blank(),axis.ticks=element_blank(),
 axis.text.x=element_text(angle=45,hjust=1,vjust=1))
export<-function(p,stem,w,h){
 target<-file.path(out,'figure');dir.create(target,recursive=TRUE,showWarnings=FALSE)
 svglite::svglite(file.path(target,paste0(stem,'.svg')),width=w/25.4,height=h/25.4);print(p);dev.off()
 ragg::agg_tiff(file.path(target,paste0(stem,'.tiff')),width=w,height=h,units='mm',res=600,compression='lzw',background='white');print(p);dev.off()
}
export(s6,'FigS6',220,208)
stopifnot(identical(before,tools::md5sum(input)))
# Figure-specific observations and complete adjusted pairwise statistics.
figure_variables<-list(FigS2=c('SDW','RDW'),FigS3=c('SCd','RCd','TF','ECd'),FigS4=c('RN','RTN','L','AD','SA','V'),FigS5=c('SPAD','Ci','gs','A','E','WUE'),FigS6=c('SDW','RDW','SCd','RCd','TF','ECd','RN','L','SA','SPAD','A','WUE'))
stat_tables<-list(group_summary_and_letters.csv=summaries,pairwise_adjusted_tests.csv=pairs,test_methods_and_diagnostics.csv=methods)
panel_traits<-list(FigS2=c(panel_b_Shoot_dry_weight='SDW',panel_c_Root_dry_weight='RDW'),FigS3=c(panel_a_Shoot_Cd='SCd',panel_b_Root_Cd='RCd',panel_c_Translocation_factor='TF',panel_d_Shoot_Cd_extraction='ECd'))
for(nm in names(figure_variables)){
 vs<-figure_variables[[nm]];dst<-file.path(data_out,nm);dir.create(dst,recursive=TRUE,showWarnings=FALSE)
 if(nm%in%names(panel_traits)){
  for(panel in names(panel_traits[[nm]])){
   d<-file.path(dst,panel);dir.create(d,recursive=TRUE,showWarnings=FALSE)
   write.csv(dat[,c('Treatment','Rep','order',panel_traits[[nm]][[panel]])],file.path(d,'data.csv'),row.names=FALSE)
  }
 }else write.csv(dat[,c('Treatment','Rep','order',vs)],file.path(dst,'data.csv'),row.names=FALSE)
 if(nm!='FigS6'){
  dir.create(file.path(dst,'results'),recursive=TRUE,showWarnings=FALSE)
  for(fn in names(stat_tables)){tb<-stat_tables[[fn]];write.csv(tb[tb$Variable%in%vs,],file.path(dst,'results',fn),row.names=FALSE)}
 }
}
bcf_dir<-file.path(common_out,'BCF_补充统计');dir.create(bcf_dir,recursive=TRUE,showWarnings=FALSE)
for(fn in names(stat_tables)){tb<-stat_tables[[fn]];write.csv(tb[tb$Variable%in%c('SBCF','RBCF'),],file.path(bcf_dir,fn),row.names=FALSE)}
message('Completed S2-S6 with all 114 observations retained; S6 uses 19 treatment means.')
)----")
 for(name in names(code_files)){p<-file.path(stage,name);dir.create(dirname(p),recursive=TRUE,showWarnings=FALSE);writeLines(code_files[[name]],p,useBytes=TRUE)}
 env<-new.env(parent=environment())
 env$commandArgs<-function(trailingOnly=FALSE)if(trailingOnly)paste0('--output=',stage)else base::commandArgs(FALSE)
 source(file.path(stage,r"----(code/figS2_S6.R)----"),local=env)
 
 for(s in specs)if(!is.null(s$target)){
  p<-file.path(stage,s$target)
  if(file.exists(p)&&!grepl('[.]json$',p)){
   d<-read.csv(p,check.names=FALSE,stringsAsFactors=FALSE,na.strings='NA',colClasses='character')
   for(j in seq_along(s$kinds)){
    if(s$kinds[[j]]=='number')d[[j]]<-as.numeric(d[[j]])
    if(s$kinds[[j]]=='boolean')d[[j]]<-as.logical(d[[j]])
   }
   tables[[s$name]]<-d
  }
 }
 stopifnot(identical(initial_hash,tools::md5sum(input_file)))
 for(ext in c('tiff','svg')){
  from<-file.path(stage,'figure',paste0(r"----(FigS6)----",'.',ext));stopifnot(file.exists(from))
  if(!file.copy(from,file.path(output_root,paste0(r"----(FigS6)----",'.',ext)),overwrite=TRUE))stop('Could not save figure.')
 }
 if(!'--no-save-data'%in%args)refresh_workbook(tables,file.path(output_root,paste0(r"----(FigS6)----",'_data.xlsx')),input_file)
 message(r"----(FigS6)----",' complete; original observations and analysis definitions preserved.')
}
run_figure()
