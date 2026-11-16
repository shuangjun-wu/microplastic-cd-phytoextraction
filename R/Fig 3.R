# Typography-only final export. Values, paths, positions and canvas are preserved.
italic_statistical_P_export <- function(svg_file,tiff_file) {
  for(pkg in c('xml2','rsvg','magick'))
    if(!requireNamespace(pkg,quietly=TRUE))stop('Missing typography/export package: ',pkg)
  doc<-xml2::read_xml(svg_file)
  nodes<-xml2::xml_find_all(doc,"//*[local-name()='text'] | //*[local-name()='tspan']")
  changed<-0L
  pattern<-"(?<![[:alnum:]_])P(?![[:alnum:]_])"
  for(el in nodes) {
    # Native SVG text nodes are single-run labels; existing math tspans are left alone.
    if(length(xml2::xml_children(el)))next
    value<-xml2::xml_text(el)
    if(!grepl(pattern,value,perl=TRUE))next
    style<-paste(xml2::xml_attr(el,'style'),xml2::xml_attr(el,'font-family'),
                 xml2::xml_attr(el,'font-style'))
    if(grepl('italic|oblique',style,ignore.case=TRUE))next
    # Avoid any upright P in protein/database IDs; figure text has only statistical uses.
    is_stat<-grepl("P[[:space:]]*(=|<|>|≤|≥|:)|Mantel P|[Rr]aw P|Welch P",value,perl=TRUE)
    if(!is_stat)next
    at<-gregexpr(pattern,value,perl=TRUE)[[1]]
    xml2::xml_set_attr(el,'xml:space','preserve')
    xml2::xml_text(el)<-''
    from<-1L
    for(i in at){
      if(i>from)xml2::xml_add_child(el,'tspan',substr(value,from,i-1L))
      p<-xml2::xml_add_child(el,'tspan','P')
      xml2::xml_set_attr(p,'font-style','italic')
      from<-i+1L;changed<-changed+1L
    }
    if(from<=nchar(value))xml2::xml_add_child(el,'tspan',substr(value,from,nchar(value)))
    for(run in xml2::xml_children(el))xml2::xml_set_attr(run,'xml:space','preserve')
  }
  if(changed){
    xml2::write_xml(doc,svg_file,options='no_declaration')
    info<-magick::image_info(magick::image_read(tiff_file))
    png<-rsvg::rsvg_png(svg_file,width=info$width,height=info$height)
    img<-magick::image_read(png)
    magick::image_write(img,path=tiff_file,format='tiff',compression='lzw',density='600x600')
  }
  message('Italic statistical P: ',changed,' glyphs; ',basename(svg_file))
  invisible(changed)
}

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
 input_file<-arg('--input=',file.path(script_dir,paste0(r"----(Fig3)----",'_data.xlsx')))
 output_root<-arg('--output=',script_dir);dir.create(output_root,recursive=TRUE,showWarnings=FALSE)
 if(!file.exists(input_file))stop('Missing workbook: ',input_file)
 input_file<-normalizePath(input_file);initial_hash<-tools::md5sum(input_file)
 pkgs<-c('readxl','jsonlite','writexl','xml2','zip')
 missing<-pkgs[!vapply(pkgs,requireNamespace,quietly=TRUE,FUN.VALUE=logical(1))]
 if(length(missing))stop('Missing R packages: ',paste(missing,collapse=', '))
 specs<-jsonlite::fromJSON(r"----([{"name":"原始数据","columns":["Treatment","S-ACP","S-β-GC","S-UE","SAHN","SAP","STN","SAK","STP","STK","STC","SDOC","pH","SCd","SACd"],"kinds":["string","number","number","number","number","number","number","number","number","number","number","number","number","number","number"],"target":"data/fig3.csv"},{"name":"运算数据","columns":["pH","DOC","TC","AN","TN","AP","TP","AK","TK","UE","BG","ACP","Total_Cd","DTPA_Cd"],"kinds":["number","number","number","number","number","number","number","number","number","number","number","number","number","number"],"target":"data/fig3b/Standardized_soil_data.csv"},{"name":"Panel_a处理效应","columns":["variable","Treatment","n_CK","n_treatment","mean_CK","SD_CK","mean_treatment","SD_treatment","mean_difference","mean_difference_CI_low","mean_difference_CI_high","change_percent","g","g_bootstrap_CI_low","g_bootstrap_CI_high","p","q_BH_28","stars","x","y","cell_label"],"kinds":["string","string","number","number","number","number","number","number","number","number","number","number","number","number","number","number","number","string","number","number","string"],"target":"data/fig3a/Fig3a_statistics.csv"},{"name":"Panel_a组内汇总","columns":["variable","Treatment","n","mean","SD","minimum","maximum"],"kinds":["string","string","number","number","number","number","number"],"target":"data/fig3a/Fig3a_group_summary.csv"},{"name":"变量对应","columns":["variable","source_column"],"kinds":["string","string"],"target":"data/fig3a/Fig3a_variable_mapping.csv"},{"name":"土壤相关性","columns":["v1","v2","x","y","rho","p","q","stars"],"kinds":["string","string","number","number","number","number","number","string"],"target":"data/fig3b/Spearman_all_pairs.csv"},{"name":"Panel_b相关矩阵","columns":["v1","v2","x","y","rho","p","q","stars","side","xmin","xmax","ymin","ymax"],"kinds":["string","string","number","number","number","number","number","string","number","number","number","number","number"],"target":"data/fig3b/Figure_correlation_cells.csv"},{"name":"土壤与Cd相关","columns":["variable","Cd","rho","p","q_BH_24"],"kinds":["string","string","number","number","number"],"target":"data/fig3b/Soil_Cd_Spearman.csv"},{"name":"Mantel检验","columns":["variable","Cd","n","r","p","p_two_sided","p_within_treatment","loo_min","loo_max","permutations","q_BH_24","q_within_BH_24"],"kinds":["string","string","number","number","number","number","number","number","number","number","number","number"],"target":"data/fig3b/Mantel_results.csv"},{"name":"Panel_b连线数据","columns":["variable","Cd","n","r","p","p_two_sided","p_within_treatment","loo_min","loo_max","permutations","q_BH_24","q_within_BH_24","target","x0","y0","x1","y1","P_group","r_group","sign","edge"],"kinds":["string","string","number","number","number","number","number","number","number","number","number","number","number","number","number","number","number","string","string","string","number"],"target":"data/fig3b/Figure_Mantel_edges.csv"}])----",simplifyVector=FALSE)
 tables<-setNames(lapply(specs,function(s){
  types<-vapply(s$kinds,function(k)switch(k,number='numeric',boolean='logical',string='text'),character(1))
  d<-as.data.frame(readxl::read_excel(input_file,sheet=s$name,col_types=types,.name_repair='minimal',na=character()),check.names=FALSE)
  stopifnot(identical(names(d),unlist(s$columns)))
  for(j in seq_along(types))if(types[j]=='text')d[[j]][is.na(d[[j]])]<-''
  d
 }),vapply(specs,`[[`,character(1),'name'))
 stage<-tempfile(paste0(r"----(Fig3)----",'_'));dir.create(stage);on.exit(unlink(stage,recursive=TRUE),add=TRUE)
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
 
 code_files<-list(r"----(code/fig3a.R)----"=r"----(# Figure 3a: all measured soil variables, treatment contrasts against CK.
# Run: Rscript /absolute/path/to/fig3a.R
# Resolve from the saved script rather than the current working directory.
resolve_script_dir <- function() {
 for(f in rev(sys.frames()))if(!is.null(f$ofile)&&file.exists(f$ofile)&&basename(f$ofile)=='fig3a.R')return(dirname(normalizePath(f$ofile)))
 a<-commandArgs(FALSE);f<-sub('^--file=','',a[grepl('^--file=',a)])
 if(length(f)&&file.exists(f[1]))return(dirname(normalizePath(f[1])))
 if(requireNamespace('rstudioapi',quietly=TRUE)&&rstudioapi::isAvailable()){
  f<-rstudioapi::getActiveDocumentContext()$path
  if(nzchar(f)&&file.exists(f))return(dirname(normalizePath(f)))
 }
 for(f in c('fig3a.R','code/fig3a.R','fig3/code/fig3a.R','R/fig3/code/fig3a.R'))if(file.exists(f))return(dirname(normalizePath(f)))
 stop('Cannot locate fig3a.R. Run Rscript with its full path or source the saved script.')
}
script_dir<-resolve_script_dir()
fig3_dir <- if(basename(script_dir)=='code')dirname(script_dir)else dirname(script_dir)
run_args <- commandArgs(trailingOnly=TRUE)
output_arg <- run_args[startsWith(run_args,'--output=')]
output_root <- if(length(output_arg))sub('^--output=','',output_arg[1])else fig3_dir
out <- file.path(output_root,'data','fig3a')
figure_dir <- file.path(output_root,'figure')
dir.create(figure_dir,recursive=TRUE,showWarnings=FALSE)
dir.create(out,recursive=TRUE,showWarnings=FALSE)
audit_outputs <- '--audit' %in% run_args
source_file <- file.path(fig3_dir, 'data', 'fig3.csv')
project <- normalizePath(file.path(fig3_dir, '../../2-论文'), mustWork = FALSE)
.libPaths(c(file.path(project, '图3更新工具/R-library'), .libPaths()))
suppressPackageStartupMessages({library(ggplot2); library(grid); library(svglite); library(ragg)})
if(audit_outputs)suppressPackageStartupMessages(library(jsonlite))
font <- 'Arial'
raw <- read.csv(source_file, check.names = FALSE, fileEncoding = 'UTF-8-BOM')
map <- c(pH='pH', DOC='SDOC', TC='STC', AN='SAHN', TN='STN', AP='SAP', TP='STP', AK='SAK', TK='STK', UE='S-UE', BG='S-β-GC', ACP='S-ACP', Total_Cd='SCd', DTPA_Cd='SACd')
stopifnot(setequal(names(raw), c('Treatment', unname(map))), nrow(raw)==18L,
          setequal(raw$Treatment, c('CK','PE0.5','PVC5')), all(table(raw$Treatment)==6L))
stopifnot(all(vapply(raw[unname(map)], function(x) is.numeric(x) && all(is.finite(x)), logical(1))))
# All 18 rows and all 14 measured variables are retained; no imputation/filtering.
soil <- data.frame(Treatment=raw$Treatment, raw[unname(map)], check.names=FALSE)
names(soil)[-1] <- names(map)
soil$replicate <- ave(seq_len(nrow(soil)), soil$Treatment, FUN=seq_along)
soil$sample_id <- paste0(soil$Treatment, '-', soil$replicate)
hedges <- function(tr, ck) {
 n1<-length(tr); n0<-length(ck); df<-n1+n0-2
 pooled<-sqrt(((n1-1)*var(tr)+(n0-1)*var(ck))/df)
 correction<-1-3/(4*df-1)
 correction*(mean(tr)-mean(ck))/pooled
}
set.seed(20261021)
B <- 5000L
res <- list(); summaries <- list(); k <- 0L
for (v in names(map)) {
 for (tr in c('CK','PE0.5','PVC5')) {
  x<-soil[soil$Treatment==tr,v]
  summaries[[length(summaries)+1L]]<-data.frame(variable=v,Treatment=tr,n=length(x),mean=mean(x),SD=sd(x),minimum=min(x),maximum=max(x))
 }
 ck<-soil[soil$Treatment=='CK',v]
 for(tr in c('PE0.5','PVC5')) {
  x<-soil[soil$Treatment==tr,v]
  bg<-replicate(B,hedges(sample(x,length(x),replace=TRUE),sample(ck,length(ck),replace=TRUE)))
  ci<-quantile(bg[is.finite(bg)],c(.025,.975),names=FALSE)
  tt<-t.test(x,ck,paired=FALSE,alternative='two.sided',var.equal=FALSE)
  k<-k+1L
  res[[k]]<-data.frame(variable=v,Treatment=tr,n_CK=length(ck),n_treatment=length(x),mean_CK=mean(ck),SD_CK=sd(ck),mean_treatment=mean(x),SD_treatment=sd(x),mean_difference=mean(x)-mean(ck),mean_difference_CI_low=tt$conf.int[1],mean_difference_CI_high=tt$conf.int[2],change_percent=if(v=='pH') NA_real_ else 100*(mean(x)/mean(ck)-1),g=hedges(x,ck),g_bootstrap_CI_low=ci[1],g_bootstrap_CI_high=ci[2],p=tt$p.value)
 }
}
effects<-do.call(rbind,res)
effects$q_BH_28<-p.adjust(effects$p,method='BH')
effects$stars<-ifelse(effects$q_BH_28<.001,'***',ifelse(effects$q_BH_28<.01,'**',ifelse(effects$q_BH_28<.05,'*','')))
# Fixed biological order, independent of magnitude or significance.
order <- c('pH','DOC','TC','AN','TN','AP','TP','AK','TK','UE','BG','ACP','Total_Cd','DTPA_Cd')
ypos <- seq_along(order)
effects$x<-match(effects$Treatment,c('PE0.5','PVC5'))
effects$y<-ypos[match(effects$variable,order)]
effects$cell_label<-paste0(ifelse(abs(effects$g)<.05,'0.0',sprintf('%+.1f',effects$g)),effects$stars)
# Only the colour mapping is asinh-transformed; g values and tests are unchanged.
# A full symmetric range avoids clipping TC or suppressing smaller effects.
limit <- ceiling(max(abs(effects$g)))
label <- c('pH','Dissolved organic carbon','Total carbon','Alkali-hydrolysable nitrogen','Total nitrogen','Available phosphorus','Total phosphorus','Available potassium','Total potassium','Urease','β-Glucosidase','Acid phosphatase','Total Cd','DTPA-extractable Cd')
p <- ggplot(effects,aes(x,y,fill=g))+
 geom_tile(width=.985,height=.985,colour='white',linewidth=.15)+
 geom_text(aes(label=cell_label),family=font,size=7.2/.pt,colour='#111111')+
 scale_x_continuous(breaks=1:2,labels=c('PE0.5','PVC5'),position='bottom',limits=c(.5,2.5),expand=c(0,0))+
 scale_y_reverse(breaks=ypos,labels=label,limits=c(14.5,.5),expand=c(0,0))+
 scale_fill_gradient2(low='#2D70AD',mid='#D7DDE2',high='#E64B35',midpoint=0,
  transform='asinh',limits=c(-limit,limit),breaks=c(-25,-5,0,5,25),
  labels=c('−25','−5','0','5','25'),name='Hedges’ g\nvs CK',
  guide=guide_colourbar(direction='vertical',title.position='top',title.hjust=0,
   barwidth=unit(2.8,'mm'),barheight=unit(35,'mm'),nbin=512,ticks=TRUE,frame.colour=NA))+
 labs(x=NULL,y=NULL,tag='a')+
 theme_minimal(base_family=font,base_size=7.5)+
 theme(panel.grid=element_blank(),axis.text=element_text(colour='#263942',size=7.5),
  axis.text.y=element_text(margin=margin(r=3)),axis.text.x=element_text(margin=margin(t=3)),
  axis.ticks=element_blank(),legend.position='right',legend.text=element_text(size=6.5),
  legend.title=element_text(size=7),legend.margin=margin(0,0,0,1),legend.box.margin=margin(0,0,0,0),legend.spacing.x=unit(2,'pt'),
  plot.margin=margin(3,3,2,2),plot.tag=element_text(family=font,face='bold',size=11),
  plot.tag.position='topleft')
width_mm<-104; height_mm<-84
stem<-file.path(figure_dir,'Fig3a')
svglite::svglite(paste0(stem,'.svg'),width=width_mm/25.4,height=height_mm/25.4,system_fonts=list(sans=font));print(p);dev.off()
ragg::agg_tiff(paste0(stem,'.tiff'),width=width_mm,height=height_mm,units='mm',res=600,compression='lzw',background='white');print(p);dev.off()
write.csv(effects,file.path(out,'Fig3a_statistics.csv'),row.names=FALSE,na='')
write.csv(do.call(rbind,summaries),file.path(out,'Fig3a_group_summary.csv'),row.names=FALSE)
write.csv(data.frame(variable=names(map),source_column=unname(map)),file.path(out,'Fig3a_variable_mapping.csv'),row.names=FALSE)
if(audit_outputs){
dir.create(file.path(out,'QA'),showWarnings=FALSE)
write_json(list(figure='Fig3a',archetype='quantitative grid',claim='All measured soil responses to PE0.5 and PVC5 relative to CK, including Cd background and extractability.',n_rows_retained=18,n_rows_excluded=0,n_variables_retained=14,n_variables_excluded=0,source_md5=unname(tools::md5sum(source_file)),colour_transform='asinh(g), symmetric limits, no clipping; labels show original g',bootstrap_seed=20261021,bootstrap_draws=B,test='Two-sided independent Welch t-test',multiplicity='BH over all 28 contrasts',width_mm=width_mm,height_mm=height_mm,font=font,alignment='NOT APPLICABLE: one panel'),file.path(out,'QA/figure_contract.json'),pretty=TRUE,auto_unbox=TRUE)
writeLines(capture.output(sessionInfo()),file.path(out,'QA/R_sessionInfo.txt'))
}
cat('Completed: 14 variables, 28 contrasts, 18 rows retained.\n')
)----",r"----(code/fig3b.R)----"=r"----(# Fig. 3b: soil-only correlation triangle with two external Cd Mantel nodes.
# Resolve from the saved script rather than the current working directory.
resolve_script_dir <- function() {
 for(f in rev(sys.frames()))if(!is.null(f$ofile)&&file.exists(f$ofile)&&basename(f$ofile)=='fig3b.R')return(dirname(normalizePath(f$ofile)))
 a<-commandArgs(FALSE);f<-sub('^--file=','',a[grepl('^--file=',a)])
 if(length(f)&&file.exists(f[1]))return(dirname(normalizePath(f[1])))
 if(requireNamespace('rstudioapi',quietly=TRUE)&&rstudioapi::isAvailable()){
  f<-rstudioapi::getActiveDocumentContext()$path
  if(nzchar(f)&&file.exists(f))return(dirname(normalizePath(f)))
 }
 for(f in c('fig3b.R','code/fig3b.R','fig3/code/fig3b.R','R/fig3/code/fig3b.R'))if(file.exists(f))return(dirname(normalizePath(f)))
 stop('Cannot locate fig3b.R. Run Rscript with its full path or source the saved script.')
}
script_dir<-resolve_script_dir()
fig3_dir<-if(basename(script_dir)=='code')dirname(script_dir)else dirname(script_dir)
run_args<-commandArgs(TRUE);output_arg<-run_args[startsWith(run_args,'--output=')]
output_root<-if(length(output_arg))sub('^--output=','',output_arg[1])else fig3_dir
out<-file.path(output_root,'data','fig3b')
figure_dir<-file.path(output_root,'figure');dir.create(figure_dir,recursive=TRUE,showWarnings=FALSE)
dir.create(out,recursive=TRUE,showWarnings=FALSE)
source_file<-file.path(fig3_dir,'data','fig3.csv');audit_outputs<-'--audit'%in%run_args
suppressPackageStartupMessages({library(ggplot2);library(grid)})
if(audit_outputs)suppressPackageStartupMessages(library(jsonlite))
font<-'Arial';ink<-'#263238'
raw<-read.csv(source_file,check.names=FALSE,fileEncoding='UTF-8-BOM')
map<-c(pH='pH',DOC='SDOC',TC='STC',AN='SAHN',TN='STN',AP='SAP',TP='STP',AK='SAK',TK='STK',UE='S-UE',BG='S-β-GC',ACP='S-ACP',Total_Cd='SCd',DTPA_Cd='SACd')
dat<-raw[unname(map)];names(dat)<-names(map)
stopifnot(nrow(dat)==18,all(table(raw$Treatment)==6),setequal(raw$Treatment,c('CK','PE0.5','PVC5')),all(vapply(dat,function(x)is.numeric(x)&&all(is.finite(x))&&sd(x)>0,logical(1))))
soil<-names(map)[1:12];cd<-c('Total_Cd','DTPA_Cd')
full<-c('pH','Dissolved organic carbon','Total carbon','Alkali-hydrolysable nitrogen','Total nitrogen','Available phosphorus','Total phosphorus','Available potassium','Total potassium','Urease','β-Glucosidase','Acid phosphatase')
star<-function(p)ifelse(p<.001,'***',ifelse(p<.01,'**',ifelse(p<.05,'*','')))
# Soil-soil: two-sided Spearman; Cd is deliberately absent from the triangle.
co<-do.call(rbind,lapply(seq_along(soil),function(i){if(i==12)return(NULL);do.call(rbind,lapply((i+1):12,function(j){tt<-cor.test(dat[[soil[i]]],dat[[soil[j]]],method='spearman',exact=FALSE);data.frame(v1=soil[i],v2=soil[j],x=j,y=i,rho=unname(tt$estimate),p=tt$p.value)}))}))
co$q<-p.adjust(co$p,'BH');co$stars<-star(co$q)
write.csv(co,file.path(out,'Spearman_all_pairs.csv'),row.names=FALSE)
diag_data<-data.frame(v1=soil,v2=soil,x=1:12,y=1:12,rho=1,p=NA_real_,q=NA_real_,stars='')
tiles<-rbind(co,diag_data)
# Square area, rather than side length, is proportional to the absolute Spearman coefficient.
tiles$side<-.88*sqrt(abs(tiles$rho));tiles$xmin<-tiles$x-tiles$side/2;tiles$xmax<-tiles$x+tiles$side/2;tiles$ymin<-tiles$y-tiles$side/2;tiles$ymax<-tiles$y+tiles$side/2
write.csv(tiles,file.path(out,'Figure_correlation_cells.csv'),row.names=FALSE)
# Directional single-variable correlations are retained in the statistics only.
sc<-do.call(rbind,lapply(cd,function(e)do.call(rbind,lapply(soil,function(v){tt<-cor.test(dat[[v]],dat[[e]],method='spearman',exact=FALSE);data.frame(variable=v,Cd=e,rho=unname(tt$estimate),p=tt$p.value)}))))
sc$q_BH_24<-p.adjust(sc$p,'BH');write.csv(sc,file.path(out,'Soil_Cd_Spearman.csv'),row.names=FALSE)
# Each soil indicator has one Mantel edge to each of the two Cd endpoints.
# Single-variable Euclidean distance is absolute between-sample difference.
Z<-scale(dat);D<-lapply(as.data.frame(Z),function(x)as.matrix(dist(x)));n<-nrow(dat);lo<-lower.tri(matrix(0,n,n))
nperm<-9999L;set.seed(20261021);perms<-replicate(nperm,sample.int(n))
within_perms<-replicate(nperm,{idx<-seq_len(n);for(g in unique(raw$Treatment)){ii<-which(raw$Treatment==g);idx[ii]<-sample(ii)};idx})
res<-list();k<-0L
for(e in cd){
 dm<-D[[e]];obs<-dm[lo];std<-function(z)as.numeric(scale(z));yo<-std(obs)
 null<-vapply(seq_len(nperm),function(i)std(dm[perms[,i],perms[,i]][lo]),numeric(sum(lo)))
 null_within<-vapply(seq_len(nperm),function(i)std(dm[within_perms[,i],within_perms[,i]][lo]),numeric(sum(lo)))
 for(v in soil){
  x<-std(D[[v]][lo]);r<-sum(x*yo)/(length(x)-1);rn<-as.numeric(crossprod(x,null))/(length(x)-1);rw<-as.numeric(crossprod(x,null_within))/(length(x)-1)
  loo<-vapply(seq_len(n),function(i){jj<-setdiff(seq_len(n),i);cor(as.dist(D[[v]][jj,jj]),as.dist(dm[jj,jj]))},numeric(1))
  k<-k+1L;res[[k]]<-data.frame(variable=v,Cd=e,n=n,r=r,p=(sum(rn>=r-1e-12)+1)/(nperm+1),p_two_sided=(sum(abs(rn)>=abs(r)-1e-12)+1)/(nperm+1),p_within_treatment=(sum(rw>=r-1e-12)+1)/(nperm+1),loo_min=min(loo),loo_max=max(loo),permutations=nperm)
 }
}
mt<-do.call(rbind,res);mt$q_BH_24<-p.adjust(mt$p,'BH');mt$q_within_BH_24<-p.adjust(mt$p_within_treatment,'BH')
write.csv(mt,file.path(out,'Mantel_results.csv'),row.names=FALSE)
write.csv(as.data.frame(Z),file.path(out,'Standardized_soil_data.csv'),row.names=FALSE)
mt$target<-match(mt$variable,soil);mt$x0<-ifelse(mt$Cd=='Total_Cd',-1.65,.35);mt$y0<-ifelse(mt$Cd=='Total_Cd',4.1,8.3)
mt$x1<-mt$target-.75;mt$y1<-mt$target+.12
mt$P_group<-factor(ifelse(mt$p<.01,'P < 0.01',ifelse(mt$p<.05,'0.01 ≤ P < 0.05',ifelse(mt$p<.1,'0.05 ≤ P < 0.10','P ≥ 0.10'))),levels=c('P < 0.01','0.01 ≤ P < 0.05','0.05 ≤ P < 0.10','P ≥ 0.10'))
mt$r_group<-factor(ifelse(abs(mt$r)<.1,'< 0.10',ifelse(abs(mt$r)<.3,'0.10–0.30','≥ 0.30')),levels=c('< 0.10','0.10–0.30','≥ 0.30'))
mt$sign<-ifelse(mt$r>=0,'Positive','Negative');mt$edge<-seq_len(nrow(mt))
# Quadratic arcs bow into the empty network half, away from the matrix cells.
paths<-do.call(rbind,lapply(seq_len(nrow(mt)),function(i){m<-mt[i,];t<-seq(0,1,length.out=90);bend<-if(m$Cd=='Total_Cd').65 else .4;cx<-(m$x0+m$x1)/2-bend;cy<-(m$y0+m$y1)/2+bend;data.frame(x=(1-t)^2*m$x0+2*(1-t)*t*cx+t^2*m$x1,y=(1-t)^2*m$y0+2*(1-t)*t*cy+t^2*m$y1,edge=m$edge,P_group=m$P_group,r_group=m$r_group,sign=m$sign)}))
paths$P_group<-factor(paths$P_group,levels=levels(mt$P_group));paths$r_group<-factor(paths$r_group,levels=levels(mt$r_group))
# Non-significant paths are drawn first so supported edges remain legible.
paths<-paths[order(paths$P_group,decreasing=TRUE),]
nodes<-data.frame(x=c(-1.65,.35),y=c(4.1,8.3),label=c('Total Cd','DTPA-extractable Cd'))
anchors<-data.frame(x=(1:12)-.75,y=(1:12)+.12)
p<-ggplot()+
 geom_path(data=paths,aes(x,y,group=edge,colour=P_group,linewidth=r_group,linetype=sign),lineend='round')+
 geom_tile(data=tiles,aes(x,y),width=.99,height=.99,fill=NA,colour='#70777B',linewidth=.25)+
 geom_rect(data=tiles,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill=rho),colour=NA)+
 geom_text(data=subset(tiles,stars!=''),aes(x,y,label=stars),family=font,size=6.4/.pt,colour='#171717')+
 geom_point(data=anchors,aes(x,y),shape=8,size=1.5,stroke=.45,colour='#E87560')+
 geom_point(data=nodes,aes(x,y),shape=8,size=2.8,stroke=.85,colour=ink)+
 geom_text(data=nodes,aes(x=x-.35,y=y,label=label),family=font,size=7/.pt,hjust=1,lineheight=.95,colour=ink)+
 scale_fill_gradient2(low='#58B4AC',mid='white',high='#F47B5E',midpoint=0,limits=c(-1,1),breaks=c(-1,-.5,0,.5,1),name='Spearman ρ',guide=guide_colourbar(order=1,barheight=unit(28,'mm'),barwidth=unit(2.8,'mm'),title.position='top'))+
 scale_colour_manual(values=c('P < 0.01'='#7EBE65','0.01 ≤ P < 0.05'='#F47B5E','0.05 ≤ P < 0.10'='#58C4C6','P ≥ 0.10'='#C1C6C8'),drop=FALSE,name='Mantel P',guide=guide_legend(order=2,override.aes=list(linewidth=.8)))+
 scale_linewidth_manual(values=c('< 0.10'=.25,'0.10–0.30'=.65,'≥ 0.30'=1.15),drop=FALSE,name='|Mantel r|',guide=guide_legend(order=3))+
 scale_linetype_manual(values=c(Positive='solid',Negative='dashed'),name='Mantel r',guide=guide_legend(order=4,keywidth=unit(7,'mm'),override.aes=list(colour='#5F696E',linewidth=.6,linetype=c('dashed','solid'))))+
 scale_x_continuous(breaks=1:12,labels=full,limits=c(-3.8,12.55),expand=c(0,0),position='top')+
 scale_y_reverse(breaks=1:12,labels=full,limits=c(12.6,.4),expand=c(0,0),position='right')+
 coord_fixed(ratio=1,clip='off')+labs(x=NULL,y=NULL,tag='b')+
 theme_minimal(base_family=font,base_size=7.2)+theme(panel.grid=element_blank(),axis.ticks=element_blank(),axis.text=element_text(size=6.7,colour=ink),axis.text.x=element_text(angle=45,hjust=0,vjust=0,margin=margin(b=3)),axis.text.y=element_text(hjust=0,margin=margin(l=4)),legend.position='right',legend.title=element_text(size=7),legend.text=element_text(size=6.3),legend.key.size=unit(3,'mm'),legend.spacing.y=unit(1.5,'mm'),legend.box.spacing=unit(3,'mm'),plot.margin=margin(3,3,3,3),plot.tag=element_text(size=11,face='bold'))
width_mm<-183;height_mm<-124;stem<-file.path(figure_dir,'Fig3b')
svglite::svglite(paste0(stem,'.svg'),width=width_mm/25.4,height=height_mm/25.4,system_fonts=list(sans=font));print(p);dev.off()
ragg::agg_tiff(paste0(stem,'.tiff'),width=width_mm,height=height_mm,units='mm',res=600,compression='lzw',background='white');print(p);dev.off()
write.csv(mt,file.path(out,'Figure_Mantel_edges.csv'),row.names=FALSE)
if(audit_outputs){
dir.create(file.path(out,'QA'),showWarnings=FALSE)
write_json(list(n=18,retained_rows=18,soil_variables=soil,external_Cd_nodes=cd,figure='Soil-only upper triangle; square area proportional to absolute Spearman rho; two external Cd nodes each linked to 12 individual soil indicators',Spearman='Two-sided, exact=FALSE; BH correction over all 66 soil-soil tests; matrix stars use q',Mantel='Pearson correlation of single-indicator Euclidean distance matrices; 9999 unrestricted label permutations; upper-tailed P shown by edge colours',Mantel_multiplicity='All 24 raw P and BH q reported',permutations=nperm,seed=20261021,within_treatment='Sensitivity only; main graph uses pooled 18 samples',note='A single-indicator distance Mantel coefficient measures agreement in between-sample differences, not the signed abundance relationship or a causal contribution. Soil-Cd Spearman directions remain in a separate table, not the triangle.',alignment='NOT APPLICABLE: one integrated plot area; coordinate ratio fixed at one',size_mm=c(width_mm,height_mm),source_md5=unname(tools::md5sum(source_file))),file.path(out,'QA/analysis_contract.json'),auto_unbox=TRUE,pretty=TRUE)
writeLines(capture.output(sessionInfo()),file.path(out,'QA/R_sessionInfo.txt'))
}
print(mt[order(mt$p),c('variable','Cd','r','p','q_BH_24')])
)----",r"----(code/fig3.R)----"=r"----(# Fig. 3: existing soil response heatmap + existing soil/Cd Mantel panel.
# This script only assembles existing statistics; it does not rerun any tests.
args <- commandArgs(trailingOnly=TRUE)
# Resolve from the saved script rather than the current working directory.
resolve_script_dir <- function() {
 for(f in rev(sys.frames()))if(!is.null(f$ofile)&&file.exists(f$ofile)&&basename(f$ofile)=='fig3.R')return(dirname(normalizePath(f$ofile)))
 a<-commandArgs(FALSE);f<-sub('^--file=','',a[grepl('^--file=',a)])
 if(length(f)&&file.exists(f[1]))return(dirname(normalizePath(f[1])))
 if(requireNamespace('rstudioapi',quietly=TRUE)&&rstudioapi::isAvailable()){
  f<-rstudioapi::getActiveDocumentContext()$path
  if(nzchar(f)&&file.exists(f))return(dirname(normalizePath(f)))
 }
 for(f in c('fig3.R','code/fig3.R','fig3/code/fig3.R','R/fig3/code/fig3.R'))if(file.exists(f))return(dirname(normalizePath(f)))
 stop('Cannot locate fig3.R. Run Rscript with its full path or source the saved script.')
}
script_dir<-resolve_script_dir()
fig3_dir <- if(basename(script_dir)=='code')dirname(script_dir)else script_dir
output_arg <- args[startsWith(args,'--output=')]
output_root <- if(length(output_arg)) sub('^--output=','',output_arg[1]) else fig3_dir
out <- file.path(output_root,'figure')
source_args <- args[!startsWith(args,'--')]
source_root <- if(length(source_args)) normalizePath(source_args[1]) else file.path(fig3_dir,'data')
dir.create(out,recursive=TRUE,showWarnings=FALSE)
audit_outputs <- '--audit' %in% args
# Default: preserve statistics and export only TIFF and editable SVG.
suppressPackageStartupMessages({library(ggplot2);library(patchwork);library(grid);library(svglite);library(ragg)})
if(audit_outputs)suppressPackageStartupMessages(library(jsonlite))
font <- 'Arial'; ink <- '#263238'
axis_pt <- 7.5; legend_pt <- 7; legend_title_pt <- 7.5
quartzFonts(Arial=quartzFont(c('Arial','Arial-BoldMT','Arial-ItalicMT','Arial-BoldItalicMT')))
effects <- read.csv(file.path(source_root,'fig3a','Fig3a_statistics.csv'),check.names=FALSE)
order <- c('pH','DOC','TC','AN','TN','AP','TP','AK','TK','UE','BG','ACP','Total_Cd','DTPA_Cd')
ypos <- seq_along(order)
tiles <- read.csv(file.path(source_root,'fig3b','Figure_correlation_cells.csv'),check.names=FALSE)
mt <- read.csv(file.path(source_root,'fig3b','Mantel_results.csv'),check.names=FALSE)
soil <- order[1:12]
full <- c('pH','Dissolved organic carbon','Total carbon','Alkali-hydrolysable nitrogen','Total nitrogen','Available phosphorus','Total phosphorus','Available potassium','Total potassium','Urease','β-Glucosidase','Acid phosphatase')
stopifnot(nrow(effects)==28L,nrow(mt)==24L,nrow(tiles)==78L)
limit <- ceiling(max(abs(effects$g)))
label <- c('pH','Dissolved organic carbon','Total carbon','Alkali-hydrolysable nitrogen','Total nitrogen','Available phosphorus','Total phosphorus','Available potassium','Total potassium','Urease','β-Glucosidase','Acid phosphatase','Total Cd','DTPA-extractable Cd')
pa <- ggplot(effects,aes(x,y,fill=g))+
 geom_tile(width=.985,height=.985,colour='white',linewidth=.15)+
 geom_text(aes(label=cell_label),family=font,size=7.2/.pt,colour='#111111')+
 scale_x_continuous(breaks=1:2,labels=c('PE0.5','PVC5'),position='bottom',limits=c(.5,2.5),expand=c(0,0))+
 scale_y_reverse(breaks=ypos,labels=label,limits=c(14.5,.5),expand=c(0,0))+
 scale_fill_gradient2(low='#2D70AD',mid='#D7DDE2',high='#E64B35',midpoint=0,
  transform='asinh',limits=c(-limit,limit),breaks=c(-25,-5,0,5,25),
  labels=c('−25','−5','0','5','25'),name='Hedges’ g\nvs CK',
  guide=guide_colourbar(direction='vertical',title.position='top',title.hjust=0,
   barwidth=unit(2.8,'mm'),barheight=unit(35,'mm'),nbin=512,ticks=TRUE,frame.colour=NA))+
 labs(x=NULL,y=NULL,tag='a')+
 theme_minimal(base_family=font,base_size=axis_pt)+
 theme(panel.grid=element_blank(),axis.text=element_text(colour='#263942',size=axis_pt),
  axis.text.y=element_text(margin=margin(r=3)),axis.text.x=element_text(margin=margin(t=3)),
  axis.ticks=element_blank(),legend.position='right',legend.text=element_text(size=legend_pt),
  legend.title=element_text(size=legend_title_pt),legend.margin=margin(0,0,0,1),legend.box.margin=margin(0,0,0,0),legend.spacing.x=unit(2,'pt'),
  plot.margin=margin(3,3,2,2),plot.tag=element_text(family=font,face='bold',size=11),
  plot.tag.position='topleft')

mt$target<-match(mt$variable,soil);mt$x0<-ifelse(mt$Cd=='Total_Cd',-1.65,.35);mt$y0<-ifelse(mt$Cd=='Total_Cd',4.1,8.3)
mt$x1<-mt$target-.75;mt$y1<-mt$target+.12
mt$P_group<-factor(ifelse(mt$p<.01,'P < 0.01',ifelse(mt$p<.05,'0.01 ≤ P < 0.05',ifelse(mt$p<.1,'0.05 ≤ P < 0.10','P ≥ 0.10'))),levels=c('P < 0.01','0.01 ≤ P < 0.05','0.05 ≤ P < 0.10','P ≥ 0.10'))
mt$r_group<-factor(ifelse(abs(mt$r)<.1,'< 0.10',ifelse(abs(mt$r)<.3,'0.10–0.30','≥ 0.30')),levels=c('< 0.10','0.10–0.30','≥ 0.30'))
mt$sign<-ifelse(mt$r>=0,'Positive','Negative');mt$edge<-seq_len(nrow(mt))
# Quadratic arcs bow into the empty network half, away from the matrix cells.
paths<-do.call(rbind,lapply(seq_len(nrow(mt)),function(i){m<-mt[i,];t<-seq(0,1,length.out=90);bend<-if(m$Cd=='Total_Cd').65 else .4;cx<-(m$x0+m$x1)/2-bend;cy<-(m$y0+m$y1)/2+bend;data.frame(x=(1-t)^2*m$x0+2*(1-t)*t*cx+t^2*m$x1,y=(1-t)^2*m$y0+2*(1-t)*t*cy+t^2*m$y1,edge=m$edge,P_group=m$P_group,r_group=m$r_group,sign=m$sign)}))
paths$P_group<-factor(paths$P_group,levels=levels(mt$P_group));paths$r_group<-factor(paths$r_group,levels=levels(mt$r_group))
# Non-significant paths are drawn first so supported edges remain legible.
paths<-paths[order(paths$P_group,decreasing=TRUE),]
nodes<-data.frame(x=c(-1.65,.35),y=c(4.1,8.3),label=c('Total Cd','DTPA-extractable Cd'))
anchors<-data.frame(x=(1:12)-.75,y=(1:12)+.12)
pb<-ggplot()+
 geom_path(data=paths,aes(x,y,group=edge,colour=P_group,linewidth=r_group,linetype=sign),lineend='round')+
 geom_tile(data=tiles,aes(x,y),width=.99,height=.99,fill=NA,colour='#70777B',linewidth=.25)+
 geom_rect(data=tiles,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill=rho),colour=NA)+
 geom_text(data=subset(tiles,stars!=''),aes(x,y,label=stars),family=font,size=6.4/.pt,colour='#171717')+
 geom_point(data=anchors,aes(x,y),shape=8,size=1.5,stroke=.45,colour='#E87560')+
 geom_point(data=nodes,aes(x,y),shape=8,size=2.8,stroke=.85,colour=ink)+
 geom_text(data=nodes,aes(x=x-.62,y=y,label=label),family=font,size=axis_pt/.pt,hjust=1,lineheight=.95,colour=ink)+
 scale_fill_gradient2(low='#58B4AC',mid='white',high='#F47B5E',midpoint=0,limits=c(-1,1),breaks=c(-1,-.5,0,.5,1),name='Spearman ρ',guide=guide_colourbar(order=1,barheight=unit(22,'mm'),barwidth=unit(2.8,'mm'),title.position='top'))+
 scale_colour_manual(values=c('P < 0.01'='#7EBE65','0.01 ≤ P < 0.05'='#F47B5E','0.05 ≤ P < 0.10'='#58C4C6','P ≥ 0.10'='#C1C6C8'),drop=FALSE,name='Mantel P',guide=guide_legend(order=2,override.aes=list(linewidth=.8)))+
 scale_linewidth_manual(values=c('< 0.10'=.25,'0.10–0.30'=.65,'≥ 0.30'=1.15),drop=FALSE,name='|Mantel r|',guide=guide_legend(order=3))+
 scale_linetype_manual(values=c(Positive='solid',Negative='dashed'),name='Mantel r',guide=guide_legend(order=4,keywidth=unit(7,'mm'),override.aes=list(colour='#5F696E',linewidth=.6,linetype=c('dashed','solid'))))+
 scale_x_continuous(breaks=1:12,labels=full,limits=c(-3.8,12.55),expand=c(0,0),position='top')+
 scale_y_reverse(breaks=1:12,labels=full,limits=c(12.6,.4),expand=c(0,0),position='right')+
 coord_fixed(ratio=1,clip='off')+labs(x=NULL,y=NULL,tag='b')+
 theme_minimal(base_family=font,base_size=7.2)+theme(panel.grid=element_blank(),axis.ticks=element_blank(),axis.text=element_text(size=axis_pt,colour=ink),axis.text.x=element_text(angle=45,hjust=0,vjust=0,margin=margin(b=3)),axis.text.y=element_text(hjust=0,margin=margin(l=4)),legend.position='right',legend.title=element_text(size=legend_title_pt),legend.text=element_text(size=legend_pt),legend.key.size=unit(3,'mm'),legend.spacing.y=unit(1.5,'mm'),legend.box.spacing=unit(3,'mm'),plot.margin=margin(3,3,3,3),plot.tag=element_text(size=11,face='bold'))

# Preserve the source panels' encodings, labels, coefficients and P values.
# Different data aspect ratios are intentional: 14-row heatmap vs square matrix.
pa <- pa+theme(plot.margin=margin(1.5,2,1.5,1.5,'mm'),legend.box.spacing=unit(1.5,'mm'))
pb <- pb+theme(legend.location='plot',legend.justification='center',plot.margin=margin(1.5,1.5,1.5,1.5,'mm'),legend.key.height=unit(2.8,'mm'),legend.spacing.y=unit(2.5,'mm'),legend.box.spacing=unit(2.5,'mm'),legend.margin=margin(0,0,0,0),legend.title=element_text(size=legend_title_pt,margin=margin(b=4)))
wmm <- 240; hmm <- 98
left_mm <- 77
# Independent physical viewports prevent one panel's diagonal axis labels from
# allocating a matching blank top strip to the other panel. Axis text uses the same
# physical font size in both panels; data aspect ratios remain unchanged.
draw_figure <- function(measure=FALSE) {
 grid::grid.newpage()
 frames <- list();data_boxes<-list()
 for(i in 1:2) {
  xpos<-if(i==1)0 else left_mm
  wi<-if(i==1)left_mm else wmm-left_mm
  pushViewport(viewport(x=unit(xpos,'mm'),y=unit(0,'mm'),width=unit(wi,'mm'),height=unit(hmm,'mm'),just=c('left','bottom'),name=paste0('component_',letters[i])))
  grid.draw(ggplotGrob(if(i==1)pa else pb))
  if(measure) {
   grid.force()
   lo<-deviceLoc(unit(0,'npc'),unit(0,'npc'),valueOnly=TRUE)
   hi<-deviceLoc(unit(1,'npc'),unit(1,'npc'),valueOnly=TRUE)
   frames[[i]]<-list(id=letters[i],bbox_pt=72*c(lo$x,lo$y,hi$x,hi$y),grid_id='component-frames',row_start=0L,row_stop=1L,col_start=i-1L,col_stop=i)
  }
  upViewport(0)
 }
 if(measure) {
  vv<-grid.ls(viewports=TRUE,grobs=FALSE,print=FALSE)
  ii<-which(grepl('^panel(?:;[^.]*)?(?:-[0-9]+)*\\.[0-9]+-[0-9]+-[0-9]+-[0-9]+$',vv$name,perl=TRUE))
  for(j in ii) {
   toks<-c(strsplit(vv$vpPath[j],'::',fixed=TRUE)[[1]],vv$name[j]);toks<-toks[toks!='ROOT' & nzchar(toks)]
   seekViewport(do.call(vpPath,as.list(toks)))
   lo<-deviceLoc(unit(0,'npc'),unit(0,'npc'),valueOnly=TRUE);hi<-deviceLoc(unit(1,'npc'),unit(1,'npc'),valueOnly=TRUE)
   upViewport(0)
   data_boxes[[length(data_boxes)+1]]<-list(viewport=paste(toks,collapse='::'),bbox_pt=72*c(lo$x,lo$y,hi$x,hi$y))
  }
  write_json(list(schema_version=1L,backend='r-grid',figure=list(width_pt=wmm/25.4*72,height_pt=hmm/25.4*72),panels=frames,
   row_groups=list(c('a','b')),column_groups=list(),exemptions=list(),
   geometry_note='Comparable physical component frames are aligned; actual data viewports are recorded separately because the heatmap and fixed-aspect matrix have different data shapes.'),
   file.path(out,'QA/panel_layout.json'),auto_unbox=TRUE,pretty=TRUE,digits=NA)
  write_json(data_boxes,file.path(out,'QA/data_viewports.json'),auto_unbox=TRUE,pretty=TRUE,digits=NA)
 }
}
if(audit_outputs){
dir.create(file.path(out,'QA'),showWarnings=FALSE)
# First final-size measurement with the same device/font as the exported PDF.
probe<-tempfile(fileext='.pdf')
quartz(type='pdf',file=probe,width=wmm/25.4,height=hmm/25.4,family='Arial');draw_figure(TRUE);dev.off();unlink(probe)
py <- '/Users/shuangjun/.cache/codex-runtimes/codex-primary-runtime/dependencies/python/bin/python3'
audit <- '/Users/shuangjun/.codex/skills/nature-figure/scripts/audit_panel_alignment.py'
status<-system2(py,c(shQuote(audit),shQuote(file.path(out,'QA/panel_layout.json')),'--json-out',shQuote(file.path(out,'QA/panel_alignment.json')),'--strict'))
stopifnot(status==0L)
}
svglite::svglite(file.path(out,'Fig3.svg'),width=wmm/25.4,height=hmm/25.4);draw_figure();dev.off()
ragg::agg_tiff(file.path(out,'Fig3.tiff'),width=wmm,height=hmm,units='mm',res=600,compression='lzw',background='white');draw_figure();dev.off()
if(audit_outputs){
write_json(list(figure='Fig3',sources=c('fig3a/Fig3a_statistics.csv','fig3b/Figure_correlation_cells.csv','fig3b/Mantel_results.csv'),
 existing_statistics_reused=TRUE,width_mm=wmm,height_mm=hmm,source_rows=c(28,78,24),font='Arial',
 layout='Soil treatment-response heatmap at left; soil/Cd correlation-Mantel panel at right; different plot-area aspect ratios are intentional.'),
 file.path(out,'QA/assembly_contract.json'),auto_unbox=TRUE,pretty=TRUE)
}
)----")
 for(name in names(code_files)){p<-file.path(stage,name);dir.create(dirname(p),recursive=TRUE,showWarnings=FALSE);writeLines(code_files[[name]],p,useBytes=TRUE)}
 env<-new.env(parent=environment())
 env$commandArgs<-function(trailingOnly=FALSE)if(trailingOnly)paste0('--output=',stage)else base::commandArgs(FALSE)
 source(file.path(stage,r"----(code/fig3a.R)----"),local=env)
 source(file.path(stage,r"----(code/fig3b.R)----"),local=env)
 source(file.path(stage,r"----(code/fig3.R)----"),local=env)
 
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
  from<-file.path(stage,'figure',paste0(r"----(Fig3)----",'.',ext));stopifnot(file.exists(from))
  if(!file.copy(from,file.path(output_root,paste0(r"----(Fig3)----",'.',ext)),overwrite=TRUE))stop('Could not save figure.')
 }
 if(!'--no-save-data'%in%args)refresh_workbook(tables,file.path(output_root,paste0(r"----(Fig3)----",'_data.xlsx')),input_file)
 message(r"----(Fig3)----",' complete; original observations and analysis definitions preserved.')
}
if ('--typography-only' %in% base::commandArgs(TRUE)) {
  here<-resolve_dir()
  a<-base::commandArgs(TRUE);b<-a[startsWith(a,'--output=')]
  target<-if(length(b))substring(b[1],10)else here
  italic_statistical_P_export(file.path(target,'Fig3.svg'),file.path(target,'Fig3.tiff'))
} else {
  run_figure()
  a<-base::commandArgs(TRUE);b<-a[startsWith(a,'--output=')]
  target<-if(length(b))substring(b[1],10)else resolve_dir()
  italic_statistical_P_export(file.path(target,'Fig3.svg'),file.path(target,'Fig3.tiff'))
}
