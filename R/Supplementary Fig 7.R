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
 input_file<-arg('--input=',file.path(script_dir,paste0(r"----(FigS7)----",'_data.xlsx')))
 output_root<-arg('--output=',script_dir);dir.create(output_root,recursive=TRUE,showWarnings=FALSE)
 if(!file.exists(input_file))stop('Missing workbook: ',input_file)
 input_file<-normalizePath(input_file);initial_hash<-tools::md5sum(input_file)
 pkgs<-c('readxl','jsonlite','writexl','xml2','zip')
 missing<-pkgs[!vapply(pkgs,requireNamespace,quietly=TRUE,FUN.VALUE=logical(1))]
 if(length(missing))stop('Missing R packages: ',paste(missing,collapse=', '))
 specs<-jsonlite::fromJSON(r"----([{"name":"原始数据","columns":["Treatment","BP (g/kg)","BFe (mg/kg)","BMg (g/kg)","BK (g/kg)","SP (g/kg)","SFe (mg/kg)","SMg (g/kg)","SK (g/kg)","RP (g/kg)","RFe (mg/kg)","RMg (g/kg)","RK (g/kg)"],"kinds":["string","number","number","number","number","number","number","number","number","number","number","number","number"],"target":"data/公共数据/figS7.csv"},{"name":"运算数据","columns":["Treatment","Rep","Variable","OrganCode","Element","Unit","Value","Organ"],"kinds":["string","number","string","string","string","string","number","string"],"target":"data/results/data_long.csv"},{"name":"分组统计及字母","columns":["Organ","Element","Unit","Variable","Panel","Treatment","n","mean","SD","SE","minimum","maximum","Letters"],"kinds":["string","string","string","string","string","string","number","number","number","number","number","number","string"],"target":"data/results/group_summary_and_letters.csv"},{"name":"两两比较","columns":["Organ","Element","Unit","Variable","Panel","group1","group2","p.adj","Method"],"kinds":["string","string","string","string","string","string","string","number","string"],"target":"data/results/pairwise_comparisons.csv"},{"name":"统计方法","columns":["Organ","Element","Unit","Variable","Panel","Method","Global_P","Minimum_Shapiro_P","Levene_P"],"kinds":["string","string","string","string","string","string","number","number","number"],"target":"data/results/statistical_methods.csv"},{"name":"Panel对应表","columns":["Panel","Organ","Element","Unit","Variable","Data_path","Observations","Replicates_per_treatment","Statistics_folder","Workbook_data_sheet","Workbook_statistics_sheet"],"kinds":["string","string","string","string","string","string","number","number","string","string","string"],"target":"data/Panel_Data_Index.csv"}])----",simplifyVector=FALSE)
 tables<-setNames(lapply(specs,function(s){
  types<-vapply(s$kinds,function(k)switch(k,number='numeric',boolean='logical',string='text'),character(1))
  d<-as.data.frame(readxl::read_excel(input_file,sheet=s$name,col_types=types,.name_repair='minimal',na=character()),check.names=FALSE)
  stopifnot(identical(names(d),unlist(s$columns)))
  for(j in seq_along(types))if(types[j]=='text')d[[j]][is.na(d[[j]])]<-''
  d
 }),vapply(specs,`[[`,character(1),'name'))
 stage<-tempfile(paste0(r"----(FigS7)----",'_'));dir.create(stage);on.exit(unlink(stage,recursive=TRUE),add=TRUE)
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
 
 code_files<-list(r"----(code/FigS7.R)----"=r"----(# Figure S7: nutrient concentrations in berry, shoot and root.
# Run the saved script using Rscript. Optional --input=... and --output=...
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
input<-arg('--input=',file.path(fig_dir,'data','公共数据','figS7.csv'))
output_root<-arg('--output=',fig_dir);out<-file.path(output_root,'figure');results<-file.path(output_root,'data','results')
invisible(lapply(c(out,results),dir.create,recursive=TRUE,showWarnings=FALSE));hash_before<-tools::md5sum(input)
suppressPackageStartupMessages({library(ggplot2);library(dplyr);library(tidyr);library(patchwork);library(grid)})
stopifnot(all(vapply(c('rstatix','car','multcompView','ragg','svglite'),requireNamespace,quietly=TRUE,FUN.VALUE=logical(1))))
font<-'Arial';ink<-'#263238';trt<-c('CK','PE0.5','PVC5')
raw<-read.csv(input,fileEncoding='UTF-8-BOM',check.names=FALSE)
stopifnot(nrow(raw)==18,all(table(raw$Treatment)==6),identical(sort(unique(raw$Treatment)),sort(trt)),ncol(raw)==13,
 all(vapply(raw[-1],function(x)is.numeric(x)&&all(is.finite(x)),logical(1))))
raw$Rep<-ave(seq_len(nrow(raw)),raw$Treatment,FUN=seq_along)
dat<-raw%>%pivot_longer(-c(Treatment,Rep),names_to='Variable',values_to='Value')%>%
 extract(Variable,c('OrganCode','Element','Unit'),regex='^([BSR])([A-Za-z]+) \\((.+)\\)$',remove=FALSE)%>%
 mutate(Treatment=factor(Treatment,levels=trt),Organ=factor(c(B='Berry',S='Shoot',R='Root')[OrganCode],levels=c('Berry','Shoot','Root')),
 Element=factor(Element,levels=c('P','Fe','Mg','K')))
stopifnot(nrow(dat)==216,!anyNA(dat),n_distinct(dat$Variable)==12)
info<-dat%>%distinct(Organ,Element,Unit,Variable)%>%arrange(Organ,Element)%>%mutate(Panel=letters[1:12])
# Reuse the original per-group Shapiro/Levene branch and its post-hoc tests.
reorder_letters<-function(v,mu){
 old<-unique(strsplit(paste(v,collapse=''),'')[[1]])
 if(length(old)<=1)return(v)
 score<-vapply(old,function(k)max(mu[grepl(k,v,fixed=TRUE)]),numeric(1));old<-old[order(-score,old)]
 map<-setNames(letters[seq_along(old)],old)
 vapply(v,function(x)paste(sort(unname(map[strsplit(x,'')[[1]]])),collapse=''),character(1))
}
analyses<-lapply(seq_len(nrow(info)),function(i){
 key<-info[i,];d<-dat[dat$Variable==key$Variable,]
 mu<-tapply(d$Value,d$Treatment,mean);sh<-vapply(split(d$Value,d$Treatment),function(x)shapiro.test(x)$p.value,numeric(1))
 lev<-car::leveneTest(Value~Treatment,data=d,center=median)$`Pr(>F)`[1]
 if(all(sh>.05)&&lev>.05){
 method<-'One-way ANOVA + Tukey HSD';fit<-aov(Value~Treatment,data=d);global<-summary(fit)[[1]][['Pr(>F)']][1]
 pw<-rstatix::tukey_hsd(d,Value~Treatment)%>%select(group1,group2,p.adj)
 }else if(all(sh>.05)){
 method<-'Welch ANOVA + Games-Howell';global<-rstatix::welch_anova_test(d,Value~Treatment)$p
 pw<-rstatix::games_howell_test(d,Value~Treatment)%>%select(group1,group2,p.adj)
 }else{
 method<-'Kruskal-Wallis + Dunn (BH)';global<-rstatix::kruskal_test(d,Value~Treatment)$p
 pw<-rstatix::dunn_test(d,Value~Treatment,p.adjust.method='BH')%>%select(group1,group2,p.adj)
 }
 if(global>.05){cl<-setNames(rep('a',3),trt)}else{
 v<-setNames(pw$p.adj,paste(pw$group1,pw$group2,sep='-'))
 cl<-multcompView::multcompLetters(v,threshold=.05)$Letters;cl<-reorder_letters(cl,mu[names(cl)])
 }
 sm<-d%>%group_by(Treatment)%>%summarise(n=n(),mean=mean(Value),SD=sd(Value),SE=sd(Value)/sqrt(n()),minimum=min(Value),maximum=max(Value),.groups='drop')
 sm$Letters<-unname(cl[as.character(sm$Treatment)])
 list(stats=cbind(key,data.frame(Method=method,Global_P=global,Minimum_Shapiro_P=min(sh),Levene_P=lev)),
 pairs=cbind(key[rep(1,nrow(pw)),],pw,Method=method),summary=cbind(key[rep(1,nrow(sm)),],sm),
 diagnostics=cbind(key[rep(1,3),],data.frame(Treatment=names(sh),Shapiro_P=unname(sh))))
})
methods<-bind_rows(lapply(analyses,'[[','stats'));pw<-bind_rows(lapply(analyses,'[[','pairs'));sm<-bind_rows(lapply(analyses,'[[','summary'))
# Verify the reproduced statistics against the accepted saved results.
reference<-file.path(fig_dir,'data','results')
oldpw<-read.csv(file.path(reference,'pairwise_comparisons.csv'))
oldst<-read.csv(file.path(reference,'statistical_methods.csv'))
oldcl<-read.csv(file.path(reference,'group_summary_and_letters.csv'))
a<-merge(pw,oldpw,by=c('Organ','Element','Unit','group1','group2'),suffixes=c('_new','_reference'))
b<-merge(methods,oldst,by=c('Organ','Element','Unit'),suffixes=c('_new','_reference'))
c<-merge(sm,oldcl,by=c('Organ','Element','Unit','Treatment'),suffixes=c('_new','_reference'))
stopifnot(nrow(a)==36,nrow(b)==12,nrow(c)==36,max(abs(a$p.adj_new-a$p.adj_reference))<1e-10,
 max(abs(b$Global_P_new-b$Global_P_reference))<1e-10,all(c$Letters_new==c$Letters_reference))
write.csv(methods,file.path(results,'statistical_methods.csv'),row.names=FALSE)
write.csv(pw,file.path(results,'pairwise_comparisons.csv'),row.names=FALSE)
write.csv(sm,file.path(results,'group_summary_and_letters.csv'),row.names=FALSE)
# Panel-specific observations; original variable names and units are retained.
for(i in seq_len(nrow(info))){
 k<-info[i,];folder<-paste0('panel_',k$Panel,'_',k$Organ,'_',k$Element)
 dest<-file.path(output_root,'data',folder);dir.create(dest,recursive=TRUE,showWarnings=FALSE)
 write.csv(dat[dat$Variable==k$Variable,],file.path(dest,'data.csv'),row.names=FALSE)
}
cols<-c(CK='#5B6770','PE0.5'='#4D78AA',PVC5='#C99837')
heads<-c(P='Phosphorus concentration\n(g kg⁻¹)',Fe='Iron concentration\n(mg kg⁻¹)',Mg='Magnesium concentration\n(g kg⁻¹)',K='Potassium concentration\n(g kg⁻¹)')
plots<-lapply(seq_len(nrow(info)),function(i){
 k<-info[i,];d<-dat[dat$Variable==k$Variable,];s<-sm[sm$Variable==k$Variable,]
 d$x<-as.numeric(d$Treatment)+c(-.15,-.09,-.03,.03,.09,.15)[d$Rep]
 spread<-diff(range(d$Value));s$y<-pmax(s$maximum,s$mean+s$SE)+.09*spread
 ggplot(d,aes(as.numeric(Treatment),Value))+
 geom_point(aes(x=x,colour=Treatment),shape=21,fill='white',size=1.35,stroke=.40,show.legend=FALSE)+
 geom_errorbar(data=s,aes(x=as.numeric(Treatment),ymin=mean-SE,ymax=mean+SE,colour=Treatment),inherit.aes=FALSE,width=.15,linewidth=.5,show.legend=FALSE)+
 geom_point(data=s,aes(x=as.numeric(Treatment),y=mean,fill=Treatment),inherit.aes=FALSE,shape=23,size=2.15,stroke=.40,colour='#263238',show.legend=FALSE)+
 geom_text(data=s,aes(x=as.numeric(Treatment),y=y,label=Letters),inherit.aes=FALSE,family=font,size=7.5/.pt,vjust=0,colour=ink)+
 scale_colour_manual(values=cols)+scale_fill_manual(values=cols)+
 scale_x_continuous(breaks=1:3,labels=trt,limits=c(.55,3.45),expand=c(0,0))+
 scale_y_continuous(n.breaks=4,expand=expansion(mult=c(.08,.18)))+
 labs(x=NULL,y=if(i%%4==1)as.character(k$Organ)else NULL,title=if(i<=4)unname(heads[as.character(k$Element)])else NULL,tag=k$Panel)+
 theme_classic(base_family=font,base_size=7.5)+theme(
 text=element_text(colour=ink),axis.text=element_text(size=7.5,colour=ink),axis.title.y=element_text(size=9,face='bold',margin=margin(r=5)),
 plot.title=element_text(size=8.5,face='plain',hjust=.5,margin=margin(b=4)),plot.tag=element_text(size=10.5,face='bold'),
 axis.line=element_line(linewidth=.30,colour=ink),axis.ticks=element_line(linewidth=.30,colour=ink),axis.ticks.length=unit(1,'mm'),
 plot.margin=margin(1.0,2.0,1.0,1.5,'mm'))
})
fig<-wrap_plots(plots,ncol=4,nrow=3)
W<-216;H<-152
make_fig_grob<-function(){
 g<-patchworkGrob(fig)
 ii<-unique(g$layout$l[grepl('^axis-l-[0-9]+$',g$layout$name)])
 stopifnot(length(ii)==4)
 widest<-Reduce(unit.pmax,lapply(ii,function(i)g$widths[i]))
 for(i in ii)g$widths[i]<-widest
 g
}
draw_fig<-function(){grid.newpage();grid.draw(make_fig_grob())}
# Final exports only; all existing coordinates and graphical settings are retained.
svglite::svglite(file.path(out,'FigS7.svg'),width=W/25.4,height=H/25.4);draw_fig();dev.off()
ragg::agg_tiff(file.path(out,'FigS7.tiff'),width=W,height=H,units='mm',res=600,compression='lzw',background='white');draw_fig();dev.off()
stopifnot(identical(hash_before,tools::md5sum(input)))
message('FigS7 redrawn; all 216 observations, test results and letters retained.')
)----")
 for(name in names(code_files)){p<-file.path(stage,name);dir.create(dirname(p),recursive=TRUE,showWarnings=FALSE);writeLines(code_files[[name]],p,useBytes=TRUE)}
 env<-new.env(parent=environment())
 env$commandArgs<-function(trailingOnly=FALSE)if(trailingOnly)paste0('--output=',stage)else base::commandArgs(FALSE)
 source(file.path(stage,r"----(code/FigS7.R)----"),local=env)
 write.csv(env$dat,file.path(stage,'data/results/data_long.csv'),row.names=FALSE)
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
  from<-file.path(stage,'figure',paste0(r"----(FigS7)----",'.',ext));stopifnot(file.exists(from))
  if(!file.copy(from,file.path(output_root,paste0(r"----(FigS7)----",'.',ext)),overwrite=TRUE))stop('Could not save figure.')
 }
 if(!'--no-save-data'%in%args)refresh_workbook(tables,file.path(output_root,paste0(r"----(FigS7)----",'_data.xlsx')),input_file)
 message(r"----(FigS7)----",' complete; original observations and analysis definitions preserved.')
}
run_figure()
