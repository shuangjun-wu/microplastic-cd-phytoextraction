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
 input_file<-arg('--input=',file.path(script_dir,paste0(r"----(Fig4)----",'_data.xlsx')))
 output_root<-arg('--output=',script_dir);dir.create(output_root,recursive=TRUE,showWarnings=FALSE)
 if(!file.exists(input_file))stop('Missing workbook: ',input_file)
 input_file<-normalizePath(input_file);initial_hash<-tools::md5sum(input_file)
 pkgs<-c('readxl','jsonlite','writexl','xml2','zip')
 missing<-pkgs[!vapply(pkgs,requireNamespace,quietly=TRUE,FUN.VALUE=logical(1))]
 if(length(missing))stop('Missing R packages: ',paste(missing,collapse=', '))
 specs<-jsonlite::fromJSON(r"----([{"name":"原始数据_RPKM","columns":["Feature","CKS_1","CKS_2","CKS_3","CKS_4","PES_1","PES_2","PES_3","PES_4","PVCS_1","PVCS_2","PVCS_3","PVCS_4"],"kinds":["string","number","number","number","number","number","number","number","number","number","number","number","number"],"target":"data/分类_属_RPKM.csv"},{"name":"运算数据_相对丰度","columns":["Feature","CKS_1","CKS_2","CKS_3","CKS_4","PES_1","PES_2","PES_3","PES_4","PVCS_1","PVCS_2","PVCS_3","PVCS_4"],"kinds":["string","number","number","number","number","number","number","number","number","number","number","number","number"],"target":"data/分类_属_相对丰度.csv"},{"name":"土壤原始数据","columns":["Treatment","S-ACP","S-β-GC","S-UE","SAHN","SAP","STN","SAK","STP","STK","STC","SDOC","pH","SCd","SACd"],"kinds":["string","number","number","number","number","number","number","number","number","number","number","number","number","number","number"],"target":"data/土壤实测数据.csv"},{"name":"样本对应","columns":["transcriptome","Trait"],"kinds":["string","string"],"target":"data/样本映射关系.csv"},{"name":"Panel_c组成","columns":["Feature","Treatment","Relative_percent"],"kinds":["string","string","number"],"target":"data/results/Composition_treatment_means.csv"},{"name":"Panel_d差异属","columns":["Item","Label","F_ANOVA","P_ANOVA","P_Welch","P_Shapiro","P_Brown_Forsythe","SD_ratio","min_P_Bonferroni","selected"],"kinds":["string","string","number","number","number","number","number","number","number","boolean"],"target":"data/results/Displayed_differential_genera.csv"},{"name":"属丰度排名","columns":["Feature","mean_relative_percent","detection_n","rank"],"kinds":["string","number","number","number"],"target":"data/results/Genus_complete_abundance_rank.csv"},{"name":"Panel_e环境相关","columns":["Unit","Feature","Label","Environment","n","rho","P_raw","rho_treatment_adjusted","P_treatment_adjusted","leave_one_out_min","leave_one_out_max","leave_one_out_same_sign","q_BH_280","q_BH_treatment_adjusted_280","stars"],"kinds":["string","string","string","string","number","number","number","number","number","number","number","number","number","number","string"],"target":"data/results/Genus_environment_Spearman_complete.csv"},{"name":"群落统计","columns":["Distance","features","pseudo_F","R2","P_PERMANOVA","P_PERMDISP_spatial_median","PCoA1_percent","PCoA2_percent","allocations"],"kinds":["string","number","number","number","number","number","number","number","number"],"target":"data/results/PCoA_PERMANOVA_PERMDISP.csv"},{"name":"Panel_b坐标","columns":["Sample","Treatment","PCoA1","PCoA2","distance_to_group_centroid_bias_adjusted"],"kinds":["string","string","number","number","number"],"target":"data/results/PCoA_reused_coordinates.csv"},{"name":"离散度方法","columns":["Method","F","P_residual_permutation_exact","allocations","bias_adjust","old_centroid_label_allocation_P"],"kinds":["string","number","number","number","boolean","number"],"target":"data/results/PERMDISP_method_check.csv"},{"name":"离散距离","columns":["Sample","Treatment","Distance_to_spatial_median","Distance_to_centroid"],"kinds":["string","string","number","number"],"target":"data/results/PERMDISP_sample_distances.csv"},{"name":"组学土壤配对","columns":["Treatment","S-ACP","S-β-GC","S-UE","SAHN","SAP","STN","SAK","STP","STK","STC","SDOC","pH","SCd","SACd","Pot","Sample","Mapping_identifier"],"kinds":["string","number","number","number","number","number","number","number","number","number","number","number","number","number","number","string","string","string"],"target":"data/results/Paired_soil_samples_and_pot_mapping.csv"},{"name":"优势属统计","columns":["Item","Label","F_ANOVA","P_ANOVA","P_Welch","P_Shapiro","P_Brown_Forsythe","SD_ratio","min_P_Bonferroni","selected"],"kinds":["string","string","number","number","number","number","number","number","number","boolean"],"target":"data/results/Top20_ANOVA_complete.csv"},{"name":"两两比较","columns":["Item","Label","group1","group2","difference","se","t","df","P_raw","P_Bonferroni","CI_Bonferroni_low","CI_Bonferroni_high"],"kinds":["string","string","string","string","number","number","number","number","number","number","number","number"],"target":"data/results/Top20_pairwise_Bonferroni_complete.csv"},{"name":"优势属样本数据","columns":["Feature","Label","Sample","Treatment","Value","Relative"],"kinds":["string","string","string","string","number","number"],"target":"data/results/Top20_sample_abundances.csv"},{"name":"Panel_a完整归属","columns":["Feature","CK","PE0.5","PVC5","Membership"],"kinds":["string","boolean","boolean","boolean","string"],"target":"data/results/Venn_complete_membership.csv"},{"name":"Panel_a交集数量","columns":["Code","Count"],"kinds":["string","number"],"target":"data/results/Venn_regions.csv"}])----",simplifyVector=FALSE)
 tables<-setNames(lapply(specs,function(s){
  types<-vapply(s$kinds,function(k)switch(k,number='numeric',boolean='logical',string='text'),character(1))
  d<-as.data.frame(readxl::read_excel(input_file,sheet=s$name,col_types=types,.name_repair='minimal',na=character()),check.names=FALSE)
  stopifnot(identical(names(d),unlist(s$columns)))
  for(j in seq_along(types))if(types[j]=='text')d[[j]][is.na(d[[j]])]<-''
  d
 }),vapply(specs,`[[`,character(1),'name'))
 stage<-tempfile(paste0(r"----(Fig4)----",'_'));dir.create(stage);on.exit(unlink(stage,recursive=TRUE),add=TRUE)
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
 
 code_files<-list(r"----(code/reanalyse.R)----"=r"----(# Recompute main-figure statistics from original profiles and saved mapping.
args <- commandArgs(trailingOnly=TRUE)
resolve_code_dir <- function() {
 for(f in rev(sys.frames())) if(!is.null(f$ofile) && file.exists(f$ofile)) return(dirname(normalizePath(f$ofile)))
 ff <- sub('^--file=','',grep('^--file=',commandArgs(),value=TRUE))
 if(length(ff) && file.exists(ff[1])) return(dirname(normalizePath(ff[1])))
 if(requireNamespace('rstudioapi',quietly=TRUE) && rstudioapi::isAvailable()) {
  ff <- rstudioapi::getActiveDocumentContext()$path
  if(nzchar(ff) && file.exists(ff)) return(dirname(normalizePath(ff)))
 }
 stop('Run the saved script using Rscript or source with its full path.')
}
code_dir <- resolve_code_dir(); fig4_dir <- dirname(code_dir)
output_arg <- args[startsWith(args,'--output=')]
output_root <- if(length(output_arg)) sub('^--output=','',output_arg[1]) else fig4_dir
audit_outputs <- '--audit' %in% args
td <- file.path(output_root,'data','results'); qa <- file.path(output_root,'QA')
dir.create(td,recursive=TRUE,showWarnings=FALSE)
if(audit_outputs) {dir.create(qa,recursive=TRUE,showWarnings=FALSE); suppressPackageStartupMessages(library(jsonlite))}
source(file.path(code_dir,'analysis.R'),local=TRUE)
)----",r"----(code/analysis.R)----"=r"----(# Existing, unmodified profiles; no imputation or observation exclusion.
samples <- c(paste0('CKS_',1:4),paste0('PES_',1:4),paste0('PVCS_',1:4))
groups <- c('CK','PE0.5','PVC5'); group <- factor(rep(groups,each=4),levels=groups)
rp_path <- file.path(fig4_dir,'data','分类_属_RPKM.csv')
rel_path <- file.path(fig4_dir,'data','分类_属_相对丰度.csv')
soil_path <- file.path(fig4_dir,'data','土壤实测数据.csv')
map_path <- file.path(fig4_dir,'data','样本映射关系.csv')
inputs <- c(rp_path,rel_path,soil_path,map_path)
input_hash <- tools::md5sum(inputs)
rp <- read.csv(rp_path,check.names=FALSE); rel <- read.csv(rel_path,check.names=FALSE)
stopifnot(identical(rp$Feature,rel$Feature),!anyDuplicated(rp$Feature))
X <- as.matrix(rp[,samples]); P <- as.matrix(rel[,samples]); rownames(X)<-rownames(P)<-rp$Feature
stopifnot(all(is.finite(X)),all(X>=0),all(is.finite(P)),all(P>=0),
 max(abs(colSums(P)-1))<1e-10,max(abs(sweep(X,2,colSums(X),'/')-P))<1e-10)
reference <- file.path(fig4_dir,'data','results')
screen_old <- read.csv(file.path(reference,'Top20_ANOVA_complete.csv'),check.names=FALSE)
names(screen_old)[names(screen_old)=='Item']<-'Feature'
ix <- order(-rowMeans(P),rownames(P))[1:20]; top <- rownames(P)[ix]
stopifnot(identical(top,screen_old$Feature))
top_labels <- screen_old$Label
rank_table <- data.frame(Feature=rownames(P),mean_relative_percent=rowMeans(P)*100,
 detection_n=rowSums(P>0));rank_table<-rank_table[order(-rank_table$mean_relative_percent,rank_table$Feature),]
rank_table$rank <- seq_len(nrow(rank_table));write.csv(rank_table,file.path(td,'Genus_complete_abundance_rank.csv'),row.names=FALSE)
raw <- do.call(rbind,lapply(seq_along(top),function(k) data.frame(Feature=top[k],Label=top_labels[k],
 Sample=samples,Treatment=group,Value=as.numeric(X[ix[k],]),Relative=as.numeric(P[ix[k],]))))
write.csv(raw,file.path(td,'Top20_sample_abundances.csv'),row.names=FALSE)

# Same ANOVA pooled-MSE contrasts as the current phylum figure; exactly 3
# pairwise comparisons in the Bonferroni family within each individual item.
test_one <- function(y,item,label) {
 d<-data.frame(Value=y,Treatment=group);fit<-aov(Value~Treatment,d);aa<-summary(fit)[[1]]
 mse<-aa[['Mean Sq']][2];df<-df.residual(fit);av<-tapply(y,group,mean);ss<-tapply(y,group,sd)
 bf<-summary(aov(abs(y-ave(y,group,FUN=median))~group))[[1]][['Pr(>F)']][1]
 pairs<-do.call(rbind,lapply(combn(groups,2,simplify=FALSE),function(gg){
 se<-sqrt(mse*(1/4+1/4));delta<-unname(av[gg[2]]-av[gg[1]]);tt<-delta/se;pp<-2*pt(-abs(tt),df)
 ci<-delta+c(-1,1)*qt(1-.05/(2*3),df)*se
 data.frame(Item=item,Label=label,group1=gg[1],group2=gg[2],difference=delta,se=se,t=tt,df=df,
 P_raw=pp,P_Bonferroni=min(1,3*pp),CI_Bonferroni_low=ci[1],CI_Bonferroni_high=ci[2])}))
 summary<-data.frame(Item=item,Label=label,F_ANOVA=aa[['F value']][1],P_ANOVA=aa[['Pr(>F)']][1],
 P_Welch=oneway.test(Value~Treatment,d,var.equal=FALSE)$p.value,
 P_Shapiro=shapiro.test(residuals(fit))$p.value,P_Brown_Forsythe=bf,SD_ratio=max(ss)/min(ss),
 min_P_Bonferroni=min(pairs$P_Bonferroni))
 list(summary=summary,pairs=pairs)
}
tests<-lapply(seq_along(top),function(k)test_one(X[ix[k],],top[k],top_labels[k]))
screen<-do.call(rbind,lapply(tests,function(z)z$summary));pairs<-do.call(rbind,lapply(tests,function(z)z$pairs))
screen$selected<-screen$P_ANOVA<.05 & screen$min_P_Bonferroni<.05
chosen<-screen[screen$selected,]
stopifnot(max(abs(screen$P_ANOVA-screen_old$P_ANOVA))<1e-10,
 max(abs(screen$min_P_Bonferroni-screen_old$min_P_Bonferroni))<1e-10)
write.csv(screen,file.path(td,'Top20_ANOVA_complete.csv'),row.names=FALSE)
write.csv(pairs,file.path(td,'Top20_pairwise_Bonferroni_complete.csv'),row.names=FALSE)
write.csv(chosen,file.path(td,'Displayed_differential_genera.csv'),row.names=FALSE)

# Top-20 is the same abundance-defined set for composition, testing and the
# environmental matrix; it was not selected by environmental correlations.
cm<-rbind(P[ix,,drop=FALSE],Other=1-colSums(P[ix,,drop=FALSE]))
composition<-do.call(rbind,lapply(groups,function(g)data.frame(Feature=rownames(cm),Treatment=g,
 Relative_percent=100*rowMeans(cm[,group==g,drop=FALSE]))))
write.csv(composition,file.path(td,'Composition_treatment_means.csv'),row.names=FALSE)
presence<-sapply(groups,function(g)rowSums(P[,group==g,drop=FALSE]>0)>0)
membership<-apply(presence,1,function(z)paste(as.integer(z),collapse=''))
region_order<-c('100','010','001','110','101','011','111')
venn_counts<-as.integer(table(factor(membership,levels=region_order)))
venn_regions<-data.frame(Code=region_order,Count=venn_counts)
write.csv(venn_regions,file.path(td,'Venn_regions.csv'),row.names=FALSE)
write.csv(data.frame(Feature=rownames(P),presence,Membership=membership,check.names=FALSE),
 file.path(td,'Venn_complete_membership.csv'),row.names=FALSE)

# Verify the existing full-profile Bray-Curtis and PCoA, then reuse coordinates.
D<-matrix(0,12,12,dimnames=list(samples,samples))
for(i in 1:11)for(j in (i+1):12)D[i,j]<-D[j,i]<-sum(abs(P[,i]-P[,j]))/sum(P[,i]+P[,j])
H<-diag(12)-matrix(1/12,12,12);ee<-eigen(-.5*H%*%(D^2)%*%H,symmetric=TRUE)
positive<-ee$values>1e-12;negative<-ee$values< -1e-12
stopifnot(!any(negative))
Y<-sweep(ee$vectors[,positive,drop=FALSE],2,sqrt(ee$values[positive]),'*')
coords<-read.csv(file.path(reference,'PCoA_reused_coordinates.csv'),check.names=FALSE)
stopifnot(identical(coords$Sample,samples),all(coords$Treatment==as.character(group)))
for(k in 1:2)stopifnot(max(abs(abs(Y[,k])-abs(coords[[paste0('PCoA',k)]])))<1e-9)
p1<-100*ee$values[1]/sum(ee$values[positive]);p2<-100*ee$values[2]/sum(ee$values[positive])

# Enumerate all 34,650 balanced allocations; no Monte Carlo tuning of P values.
allocation<-list();kk<-0L
for(a in combn(1:12,4,simplify=FALSE))for(b in combn(setdiff(1:12,a),4,simplify=FALSE)){
 kk<-kk+1L;allocation[[kk]]<-c(a,b,setdiff(1:12,c(a,b)))}
allocation<-do.call(rbind,allocation);stopifnot(nrow(allocation)==34650L)
sq<-D^2;total_ss<-sum(sq)/24
within<-function(idx)sum(vapply(0:2,function(g)sum(sq[idx[g*4+1:4],idx[g*4+1:4]])/8,numeric(1)))
w0<-within(1:12);F0<-(total_ss-w0)/2/(w0/9);R2<-(total_ss-w0)/total_ss
null_f<-apply(allocation,1,function(idx){w<-within(idx);(total_ss-w)/2/(w/9)})
permanova_p<-mean(null_f>=F0-1e-10)
accepted_pc<-read.csv(file.path(reference,'PCoA_PERMANOVA_PERMDISP.csv'),check.names=FALSE)
accepted_disp<-read.csv(file.path(reference,'PERMDISP_method_check.csv'),check.names=FALSE)
old_pc<-data.frame(P_exact=accepted_pc$P_PERMANOVA,R2=accepted_pc$R2,dispersion_P_exact=accepted_disp$old_centroid_label_allocation_P[1])
stopifnot(abs(permanova_p-old_pc$P_exact)<1e-10,abs(R2-old_pc$R2)<1e-10)

# PERMDISP: spatial median, bias correction, full-model residual permutation.
# Algorithm follows vegan betadisper / permutest.betadisper, Anderson (2006):
# https://vegandevs.github.io/vegan/reference/betadisper.html
# https://github.com/vegandevs/vegan/blob/master/R/permutest.betadisper.R
# Unlike the old centroid + label-allocation result, the statistic and null
# both use the explicitly documented median/residual procedure here.
spatial_median<-function(a){
 m<-colMeans(a)
 for(it in 1:20000){dd<-sqrt(rowSums(sweep(a,2,m,'-')^2));stopifnot(min(dd)>1e-14)
 w<-1/dd;new<-colSums(a*w)/sum(w);if(max(abs(new-m))<1e-12){m<-new;break};m<-new}
 check<-optim(m,function(v)sum(sqrt(rowSums(sweep(a,2,v,'-')^2))),method='BFGS',control=list(reltol=1e-12))
 stopifnot(abs(sum(sqrt(rowSums(sweep(a,2,m,'-')^2)))-check$value)<1e-8);m}
zmed<-zcent<-numeric(12)
for(g in groups){ii<-which(group==g);a<-Y[ii,,drop=FALSE]
 zmed[ii]<-sqrt(rowSums(sweep(a,2,spatial_median(a),'-')^2))*sqrt(4/3)
 zcent[ii]<-sqrt(rowSums(sweep(a,2,colMeans(a),'-')^2))*sqrt(4/3)}
f_anova<-function(y){bt<-sum(4*(tapply(y,group,mean)-mean(y))^2);(bt/2)/((sum((y-mean(y))^2)-bt)/9)}
null_from_alloc<-function(y){
 ym<-matrix(y[allocation],nrow=nrow(allocation));means<-cbind(rowMeans(ym[,1:4]),rowMeans(ym[,5:8]),rowMeans(ym[,9:12]))
 bt<-4*rowSums((means-mean(y))^2);(bt/2)/((sum((y-mean(y))^2)-bt)/9)}
disp<-do.call(rbind,lapply(c('spatial_median','centroid'),function(type){
 zz<-if(type=='spatial_median')zmed else zcent;rs<-zz-ave(zz,group,FUN=mean);ff<-f_anova(zz)
 data.frame(Method=type,F=ff,P_residual_permutation_exact=mean(null_from_alloc(rs)>=ff-sqrt(.Machine$double.eps)),
 allocations=34650L,bias_adjust=TRUE)}))
disp$old_centroid_label_allocation_P<-old_pc$dispersion_P_exact
dispersion_p<-disp$P_residual_permutation_exact[1]
write.csv(disp,file.path(td,'PERMDISP_method_check.csv'),row.names=FALSE)
write.csv(data.frame(Sample=samples,Treatment=group,Distance_to_spatial_median=zmed,
 Distance_to_centroid=zcent),file.path(td,'PERMDISP_sample_distances.csv'),row.names=FALSE)
community<-data.frame(Distance='Bray-Curtis',features=nrow(P),pseudo_F=F0,R2=R2,P_PERMANOVA=permanova_p,
 P_PERMDISP_spatial_median=dispersion_p,PCoA1_percent=p1,PCoA2_percent=p2,allocations=34650)
write.csv(community,file.path(td,'PCoA_PERMANOVA_PERMDISP.csv'),row.names=FALSE)
write.csv(coords,file.path(td,'PCoA_reused_coordinates.csv'),row.names=FALSE)
ell<-do.call(rbind,lapply(groups,function(g){v<-as.matrix(coords[coords$Treatment==g,c('PCoA1','PCoA2')]);th<-seq(0,2*pi,length.out=241)
 xy<-sweep(sqrt(qchisq(.95,2))*cbind(cos(th),sin(th))%*%chol(cov(v)),2,colMeans(v),'+')
 data.frame(Treatment=g,PCoA1=xy[,1],PCoA2=xy[,2])}))

# Pair each microbial sample to the recorded pot number; never use row-order
# 1:4 as a substitute for the experiment's actual 6-pot mapping.
soil<-read.csv(soil_path,check.names=FALSE,fileEncoding='UTF-8-BOM')
mp<-read.csv(map_path,check.names=FALSE,fileEncoding='UTF-8-BOM')
soil$Pot<-paste0(soil$Treatment,'-',ave(seq_len(nrow(soil)),soil$Treatment,FUN=seq_along))
expr_id<-c(paste0('CKS-',1:4),paste0('PE0.5S-',1:4),paste0('PVC5S-',1:4))
pot<-mp$Trait[match(expr_id,mp$transcriptome)];stopifnot(!anyNA(pot),!anyDuplicated(pot))
env<-soil[match(pot,soil$Pot),];stopifnot(all(env$Treatment==as.character(group)),!anyNA(env))
env$Sample<-samples;env$Mapping_identifier<-expr_id
env_vars<-c('DTPA-extractable Cd','Total Cd','pH','Dissolved organic carbon','Total carbon',
 'Alkali-hydrolysable nitrogen','Total nitrogen','Available phosphorus','Total phosphorus',
 'Available potassium','Total potassium','Urease','β-Glucosidase','Acid phosphatase')
env_cols<-c('SACd','SCd','pH','SDOC','STC','SAHN','STN','SAP','STP','SAK','STK','S-UE','S-β-GC','S-ACP')
E<-as.matrix(env[,env_cols]);colnames(E)<-env_vars
stopifnot(nrow(E)==12,all(is.finite(E)),all(apply(E,2,sd)>0))
write.csv(env,file.path(td,'Paired_soil_samples_and_pot_mapping.csv'),row.names=FALSE)
association<-do.call(rbind,lapply(c('Relative abundance','RPKM'),function(unit)do.call(rbind,lapply(1:20,function(k){
 x<-if(unit=='RPKM')X[ix[k],] else P[ix[k],]
 do.call(rbind,lapply(1:ncol(E),function(j){y<-E[,j];tt<-cor.test(x,y,method='spearman',exact=FALSE)
 rx<-residuals(lm(rank(x)~group));ry<-residuals(lm(rank(y)~group));rr<-cor(rx,ry)
 df<-12-2-2;ptreat<-2*pt(-abs(rr)*sqrt(df/max(1e-15,1-rr^2)),df)
 leave<-vapply(1:12,function(i)cor(x[-i],y[-i],method='spearman'),numeric(1))
 data.frame(Unit=unit,Feature=top[k],Label=top_labels[k],Environment=env_vars[j],n=12,
 rho=unname(tt$estimate),P_raw=tt$p.value,rho_treatment_adjusted=rr,P_treatment_adjusted=ptreat,
 leave_one_out_min=min(leave),leave_one_out_max=max(leave),
 leave_one_out_same_sign=sum(sign(leave)==sign(tt$estimate)))
 }))}))))
association$q_BH_280<-ave(association$P_raw,association$Unit,FUN=function(p)p.adjust(p,'BH'))
association$q_BH_treatment_adjusted_280<-ave(association$P_treatment_adjusted,association$Unit,FUN=function(p)p.adjust(p,'BH'))
association$stars<-ifelse(association$P_raw<.001,'***',ifelse(association$P_raw<.01,'**',ifelse(association$P_raw<.05,'*','')))
write.csv(association,file.path(td,'Genus_environment_Spearman_complete.csv'),row.names=FALSE)
stopifnot(identical(input_hash,tools::md5sum(inputs)))
if(audit_outputs) write_json(list(Input_unchanged=TRUE,Input_MD5=as.list(input_hash),Samples=samples,Pot_mapping=as.list(setNames(pot,samples)),
 Top20_relative_percent=sum(rowMeans(P[ix,]))*100,Genus_annotation_rows=nrow(P),Venn_counts=as.list(setNames(venn_counts,region_order)),
 Differential_genera=chosen$Label,Community=community,
 Correlation_main_unit='sample-wise relative abundance; full profiles closed before selecting top20',
 Correlation_stars='raw two-sided Spearman P; BH results and treatment-adjusted rank correlations in complete tables',
 No_values_imputed=TRUE,No_samples_removed=TRUE),file.path(qa,'data_analysis_check.json'),auto_unbox=TRUE,pretty=TRUE,digits=NA)
message('Analysis complete; differential genera: ',nrow(chosen),'; PERMDISP spatial median P = ',dispersion_p)
)----",r"----(code/genus_panels.R)----"=r"----(# Reuse established plot construction with existing saved statistics only.
font<-'Arial';axis_pt<-7.5;title_pt<-8.5;annotation_pt<-7;tag_pt<-11
quartzFonts(Arial=quartzFont(c('Arial','Arial-BoldMT','Arial-ItalicMT','Arial-BoldItalicMT')))
colours<-c(CK='#59636B',PE0.5='#00A087',PVC5='#E64B35')
palette<-c('#E64B35','#4DBBD5','#00A087','#3C5488','#F39B7F','#8491B4','#91D1C2','#DC0000','#7E6148','#B09C85',
 '#0091AD','#A23E8C','#2C7A3F','#E9A23B','#6E5AA6','#D77A3E','#6D8E34','#D45087','#3977A8','#7F6F55')
theme_set(theme_classic(base_family=font,base_size=axis_pt)+theme(
 axis.title=element_text(size=title_pt),axis.text=element_text(size=axis_pt,colour='black'),
 axis.line=element_line(linewidth=.3),axis.ticks=element_line(linewidth=.3),
 axis.title.x=element_text(margin=margin(t=4)),axis.title.y=element_text(margin=margin(r=4)),
 legend.text=element_text(size=axis_pt),legend.title=element_text(size=title_pt),
 legend.position='right',legend.key.height=unit(3,'mm'),legend.key.width=unit(3,'mm'),
 plot.title=element_text(size=title_pt),plot.tag=element_text(size=tag_pt,face='bold'),
 plot.margin=margin(2,2,2,2,'mm'),plot.background=element_rect(fill='white',colour=NA)))

centres<-data.frame(Treatment=groups,cx=c(-.55,.55,0),cy=c(.35,.35,-.55))
circle<-do.call(rbind,lapply(1:3,function(i){th<-seq(0,2*pi,length.out=401)
 data.frame(Treatment=groups[i],x=centres$cx[i]+1.1*cos(th),y=centres$cy[i]+1.1*sin(th))}))
vlab<-data.frame(Count=venn_counts,x=c(-1.05,1.05,0,0,-.63,.63,0),y=c(.65,.65,-1.13,.94,-.50,-.50,.09))
glab<-data.frame(Treatment=groups,x=c(-1.04,1.04,0),y=c(1.60,1.60,-1.86))
venn<-ggplot()+geom_polygon(data=circle,aes(x,y,group=Treatment,fill=Treatment),alpha=.2,colour=NA)+
 geom_path(data=circle,aes(x,y,group=Treatment,colour=Treatment),linewidth=.42)+
 geom_text(data=vlab,aes(x,y,label=Count),family=font,size=axis_pt/.pt)+
 geom_text(data=glab,aes(x,y,label=Treatment),family=font,size=axis_pt/.pt)+
 scale_fill_manual(values=colours,guide='none')+scale_colour_manual(values=colours,guide='none')+
 coord_equal(xlim=c(-1.78,1.78),ylim=c(-2,1.82),expand=FALSE,clip='off')+
 theme_void(base_family=font,base_size=axis_pt)+theme(plot.margin=margin(3,3,3,3,'mm'))

coords$Treatment<-factor(coords$Treatment,levels=groups)
pc<-ggplot(coords,aes(PCoA1,PCoA2,colour=Treatment,shape=Treatment))+
 geom_path(data=ell,aes(group=Treatment),linewidth=.5,show.legend=FALSE)+geom_point(size=2.2,stroke=.55)+
 scale_colour_manual(values=colours)+scale_shape_manual(values=c(16,17,15))+
 scale_x_continuous(expand=expansion(mult=.07),n.breaks=4)+
 scale_y_continuous(expand=expansion(mult=c(.07,.30)),n.breaks=4)+
 annotate('text',x=-Inf,y=Inf,hjust=-.03,vjust=1.15,
 label=sprintf('PERMANOVA\nR² = %.3f; P = %.4f\nPERMDISP P = %.4f',R2,permanova_p,dispersion_p),
 size=annotation_pt/.pt,lineheight=1.12,family=font,colour='black')+
 coord_equal()+labs(x=sprintf('PCoA 1 (%.1f%%)',p1),y=sprintf('PCoA 2 (%.1f%%)',p2),colour=NULL,shape=NULL)+
 theme(panel.border=element_rect(fill=NA,linewidth=.35),axis.line=element_blank(),
 legend.position='inside',legend.position.inside=c(.985,.985),legend.justification=c(1,1),
 legend.direction='vertical',legend.background=element_rect(fill='white',colour=NA),legend.margin=margin(0,0,0,0))

composition$Feature<-factor(composition$Feature,levels=c(top,'Other'))
composition$Treatment<-factor(composition$Treatment,levels=rev(groups))
fillcols<-setNames(c(palette,'#C7C7C7'),c(top,'Other'))
comp<-ggplot(composition,aes(Relative_percent,Treatment,fill=Feature))+
 geom_col(width=.76,colour='white',linewidth=.12,position=position_stack(reverse=TRUE))+
 scale_fill_manual(values=fillcols,breaks=names(fillcols),labels=c(top_labels,'Other taxa'),drop=FALSE)+
 scale_x_continuous(breaks=c(0,25,50,75,100),limits=c(0,100),expand=c(0,0))+
 scale_y_discrete(expand=expansion(add=.3))+labs(x='Relative abundance (%)',y=NULL,fill=NULL)+
 guides(fill=guide_legend(ncol=3,byrow=TRUE))+
 theme(legend.position='bottom',legend.title=element_blank(),legend.key.width=unit(2.5,'mm'),
 legend.key.height=unit(2.5,'mm'),legend.spacing.x=unit(1.2,'mm'),legend.spacing.y=unit(.6,'mm'),
 legend.box.spacing=unit(1.3,'mm'),legend.margin=margin(0,0,0,0),plot.margin=margin(2,4,2,2,'mm'))

# Bracket positions are derived from the upper data/SD extent; all observations
# remain visible. Overall ANOVA P is in tables, not on the pairwise bar panels.
make_brackets<-function(raw_data,pair_table,selected){
 sig<-pair_table[pair_table$Item%in%selected & pair_table$P_Bonferroni<.05,]
 sig$x1<-match(sig$group1,groups);sig$x2<-match(sig$group2,groups)
 sig<-sig[order(match(sig$Item,selected),sig$x2-sig$x1,sig$x1),]
 sig$y<-sig$tip<-numeric(nrow(sig))
 for(item in selected){ii<-which(sig$Item==item);if(!length(ii))next
 dd<-raw_data[raw_data$Item==item,];v<-tapply(dd$Value,dd$Treatment,mean)+tapply(dd$Value,dd$Treatment,sd)
 ymax<-max(dd$Value,v);span<-diff(range(dd$Value));span<-max(span,ymax*.12)
 sig$y[ii]<-ymax+span*(.35+.8*(seq_along(ii)-1));sig$tip[ii]<-span*.05}
 sig$label<-sprintf('P = %.4f',sig$P_Bonferroni);sig}
bar_raw<-raw[raw$Feature%in%chosen$Item,];bar_raw$Item<-bar_raw$Feature
bar_raw$Treatment<-factor(bar_raw$Treatment,levels=groups);bar_raw$Item<-factor(bar_raw$Item,levels=chosen$Item)
bar_raw$x<-as.numeric(bar_raw$Treatment)+rep(c(-.09,-.03,.03,.09),nrow(bar_raw)/4)
bar_mean<-do.call(rbind,lapply(chosen$Item,function(f)do.call(rbind,lapply(groups,function(g){v<-bar_raw$Value[bar_raw$Item==f & bar_raw$Treatment==g]
 data.frame(Item=f,Treatment=g,mean=mean(v),sd=sd(v))}))))
bar_mean$Treatment<-factor(bar_mean$Treatment,levels=groups);bar_mean$Item<-factor(bar_mean$Item,levels=chosen$Item)
bar_sig<-make_brackets(bar_raw,pairs,chosen$Item);bar_sig$Item<-factor(bar_sig$Item,levels=chosen$Item)
bar_labels<-setNames(sub('^Candidatus ','Candidatus\n',chosen$Label),chosen$Item)
bars<-ggplot(bar_mean,aes(as.numeric(Treatment),mean,fill=Treatment))+
 geom_col(width=.64)+geom_errorbar(aes(ymin=pmax(0,mean-sd),ymax=mean+sd),width=.18,linewidth=.35)+
 geom_point(data=bar_raw,aes(x=x,y=Value),inherit.aes=FALSE,shape=21,fill='white',colour='#202020',size=1.6,stroke=.4)+
 geom_segment(data=bar_sig,aes(x=x1,xend=x2,y=y,yend=y),inherit.aes=FALSE,linewidth=.3)+
 geom_segment(data=bar_sig,aes(x=x1,xend=x1,y=y,yend=y-tip),inherit.aes=FALSE,linewidth=.3)+
 geom_segment(data=bar_sig,aes(x=x2,xend=x2,y=y,yend=y-tip),inherit.aes=FALSE,linewidth=.3)+
 geom_text(data=bar_sig,aes(x=(x1+x2)/2,y=y+tip*2,label=label),inherit.aes=FALSE,
 family=font,size=annotation_pt/.pt,vjust=0)+
 scale_fill_manual(values=colours,guide='none')+
 scale_x_continuous(breaks=1:3,labels=groups,limits=c(.45,3.55),expand=c(0,0))+
 scale_y_continuous(labels=function(x)formatC(x,format='f',digits=0,big.mark=','),n.breaks=4,expand=expansion(mult=c(0,.14)))+
 facet_wrap(~Item,ncol=2,scales='free_y',axes='all_x',axis.labels='all_x',labeller=as_labeller(bar_labels))+
 labs(x=NULL,y='RPKM')+theme(strip.background=element_blank(),strip.text=element_text(size=title_pt,lineheight=1.05),
 axis.text.x=element_text(angle=45,hjust=1,vjust=1),panel.spacing.x=unit(4,'mm'),panel.spacing.y=unit(5,'mm'))

assoc_plot<-function(unit){a<-association[association$Unit==unit,]
 a$Environment<-factor(a$Environment,levels=env_vars);a$Label<-factor(a$Label,levels=rev(top_labels))
 a$number<-sprintf('%.2f',a$rho)
 ggplot(a,aes(Environment,Label,fill=rho))+
 geom_tile(colour='white',linewidth=.3)+
 geom_text(aes(label=number),family=font,size=annotation_pt/.pt,nudge_y=.13)+
 geom_text(aes(label=stars),family=font,size=annotation_pt/.pt,nudge_y=-.25)+
 scale_fill_gradient2(low='#3479B9',mid='white',high='#EF4B35',midpoint=0,limits=c(-1,1),
 breaks=c(-1,-.5,0,.5,1),name='Spearman ρ',guide=guide_colourbar(barheight=unit(30,'mm'),barwidth=unit(3,'mm')))+
 scale_x_discrete(expand=c(0,0))+scale_y_discrete(expand=c(0,0))+
 labs(x=NULL,y=NULL)+theme_minimal(base_family=font,base_size=axis_pt)+
 theme(axis.text=element_text(size=axis_pt,colour='black'),axis.text.x=element_text(angle=45,hjust=1,vjust=1,margin=margin(t=4)),
 axis.text.y=element_text(margin=margin(r=4)),legend.position='right',legend.title=element_text(size=title_pt),
 legend.text=element_text(size=axis_pt),panel.grid=element_blank(),plot.margin=margin(2,2,2,2,'mm'))}
matrix_rel<-assoc_plot('Relative abundance')
)----",r"----(code/Fig4_layout.R)----"=r"----(# Assemble the accepted Figure 4 without rerunning statistical tests.
args <- commandArgs(trailingOnly=TRUE)
resolve_code_dir <- function() {
 for(f in rev(sys.frames())) if(!is.null(f$ofile) && file.exists(f$ofile)) return(dirname(normalizePath(f$ofile)))
 ff <- sub('^--file=','',grep('^--file=',commandArgs(),value=TRUE))
 if(length(ff) && file.exists(ff[1])) return(dirname(normalizePath(ff[1])))
 if(requireNamespace('rstudioapi',quietly=TRUE) && rstudioapi::isAvailable()) {
  ff <- rstudioapi::getActiveDocumentContext()$path
  if(nzchar(ff) && file.exists(ff)) return(dirname(normalizePath(ff)))
 }
 stop('Run the saved script using Rscript or source with its full path.')
}
code_dir <- resolve_code_dir(); fig4_dir <- dirname(code_dir)
output_arg <- args[startsWith(args,'--output=')]
output_root <- if(length(output_arg)) sub('^--output=','',output_arg[1]) else fig4_dir
audit_outputs <- '--audit' %in% args
out <- file.path(output_root,'figure'); td <- file.path(fig4_dir,'data','results')
dir.create(out,recursive=TRUE,showWarnings=FALSE)
suppressPackageStartupMessages({library(ggplot2);library(patchwork);library(grid);library(svglite);library(ragg)})
read_saved<-function(f)read.csv(file.path(td,f),check.names=FALSE)
csvs<-list.files(td,full.names=TRUE,pattern='[.]csv$');before<-tools::md5sum(csvs)
groups<-c('CK','PE0.5','PVC5')
screen<-read_saved('Top20_ANOVA_complete.csv');top<-screen$Item;top_labels<-screen$Label
chosen<-read_saved('Displayed_differential_genera.csv');raw<-read_saved('Top20_sample_abundances.csv')
pairs<-read_saved('Top20_pairwise_Bonferroni_complete.csv');composition<-read_saved('Composition_treatment_means.csv')
venn_counts<-read_saved('Venn_regions.csv')$Count;coords<-read_saved('PCoA_reused_coordinates.csv')
samples<-coords$Sample;zz<-read_saved('PCoA_PERMANOVA_PERMDISP.csv')
R2<-zz$R2;permanova_p<-zz$P_PERMANOVA;dispersion_p<-zz$P_PERMDISP_spatial_median
p1<-zz$PCoA1_percent;p2<-zz$PCoA2_percent
ell<-do.call(rbind,lapply(groups,function(g){v<-as.matrix(coords[coords$Treatment==g,c('PCoA1','PCoA2')]);th<-seq(0,2*pi,length.out=241)
 xy<-sweep(sqrt(qchisq(.95,2))*cbind(cos(th),sin(th))%*%chol(cov(v)),2,colMeans(v),'+')
 data.frame(Treatment=g,PCoA1=xy[,1],PCoA2=xy[,2])}))
association<-read_saved('Genus_environment_Spearman_complete.csv')
env_vars<-unique(association$Environment)
source(file.path(code_dir,'genus_panels.R'),local=TRUE)
tagged<-function(p,letter)p+labs(tag=NULL)
# Use a wider rectangular PCoA viewport in the existing upper-left region.
# Coordinate values, ellipse coordinates and all statistics are unchanged.
pc<-pc+coord_cartesian()+scale_y_continuous(expand=expansion(mult=c(.07,.45)),n.breaks=4)
# CSV round-off may place a cumulative percentage infinitesimally above 100.
# Retain every segment; the view is 0–100, with no scale-based data removal.
comp<-comp+scale_x_continuous(breaks=c(0,25,50,75,100),expand=c(0,0))+coord_cartesian(xlim=c(0,100))
# Preserve the matrix's two-line coefficient/star format with clear spacing.
matrix_rel$layers[[2]]$position<-position_nudge(y=.22)
matrix_rel$layers[[3]]$position<-position_nudge(y=-.25)
matrix_rel$layers[[3]]$aes_params$size<-6.4/.pt
# Retain the existing composition-panel location and left-axis alignment.
comp<-comp+theme(plot.margin=margin(2,4,2,9,'mm'))
plot_objects<-list(a=tagged(venn,'a'),b=tagged(pc,'b'),c=tagged(comp,'c'),d=tagged(bars,'d'),e=tagged(matrix_rel,'e'))
wmm<-240;hmm<-264;upper_height<-117;gap_mm<-2;lower_height<-145
frames<-data.frame(id=c('a','b','c','d','e'),x=c(0,60,0,146,0),y=c(210,210,146,147,0),
 width=c(60,86,146,94,240),height=c(54,54,64,117,145))
# Independent physical component frames preserve each plot's native 7–8.5 pt
# type sizes. The lower 20×14 matrix spans the entire figure width.
draw_layout<-function(measure=FALSE){
 grid.newpage()
 for(i in seq_len(nrow(frames))){f<-frames[i,]
  pushViewport(viewport(x=unit(f$x,'mm'),y=unit(f$y,'mm'),width=unit(f$width,'mm'),height=unit(f$height,'mm'),just=c('left','bottom'),name=paste0('component_',f$id)))
  grid.draw(ggplotGrob(plot_objects[[f$id]]))
  grid.text(f$id,x=unit(1,'mm'),y=unit(1,'npc')-unit(1,'mm'),just=c('left','top'),gp=gpar(fontfamily='Arial',fontsize=11,fontface='bold'))
  upViewport(0)
 }
}
svglite(file.path(out,'Fig4.svg'),width=wmm/25.4,height=hmm/25.4);draw_layout();dev.off()
agg_tiff(file.path(out,'Fig4.tiff'),width=wmm,height=hmm,units='mm',res=600,compression='lzw',background='white');draw_layout();dev.off()
stopifnot(identical(before,tools::md5sum(csvs)))
)----")
 for(name in names(code_files)){p<-file.path(stage,name);dir.create(dirname(p),recursive=TRUE,showWarnings=FALSE);writeLines(code_files[[name]],p,useBytes=TRUE)}
 env<-new.env(parent=environment())
 env$commandArgs<-function(trailingOnly=FALSE)if(trailingOnly)paste0('--output=',stage)else base::commandArgs(FALSE)
 source(file.path(stage,'code/reanalyse.R'),local=env)
 source(file.path(stage,'code/Fig4_layout.R'),local=env)
 
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
  from<-file.path(stage,'figure',paste0(r"----(Fig4)----",'.',ext));stopifnot(file.exists(from))
  if(!file.copy(from,file.path(output_root,paste0(r"----(Fig4)----",'.',ext)),overwrite=TRUE))stop('Could not save figure.')
 }
 if(!'--no-save-data'%in%args)refresh_workbook(tables,file.path(output_root,paste0(r"----(Fig4)----",'_data.xlsx')),input_file)
 message(r"----(Fig4)----",' complete; original observations and analysis definitions preserved.')
}
if ('--typography-only' %in% base::commandArgs(TRUE)) {
  here<-resolve_dir()
  a<-base::commandArgs(TRUE);b<-a[startsWith(a,'--output=')]
  target<-if(length(b))substring(b[1],10)else here
  italic_statistical_P_export(file.path(target,'Fig4.svg'),file.path(target,'Fig4.tiff'))
} else {
  run_figure()
  a<-base::commandArgs(TRUE);b<-a[startsWith(a,'--output=')]
  target<-if(length(b))substring(b[1],10)else resolve_dir()
  italic_statistical_P_export(file.path(target,'Fig4.svg'),file.path(target,'Fig4.tiff'))
}
