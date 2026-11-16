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
  template_rows<-xml2::xml_find_all(template,"//*[local-name()='sheetData']/*[local-name()='row']")
  heights<-setNames(xml2::xml_attr(template_rows,'ht'),xml2::xml_attr(template_rows,'r'))
  updated_heights<-unname(heights[xml2::xml_attr(rows,'r')]);updated_heights[is.na(updated_heights)]<-'20'
  xml2::xml_set_attr(rows,'ht',updated_heights)
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
 input_file<-arg('--input=',file.path(script_dir,paste0(r"----(FigS8)----",'_data.xlsx')))
 output_root<-arg('--output=',script_dir);dir.create(output_root,recursive=TRUE,showWarnings=FALSE)
 if(!file.exists(input_file))stop('Missing workbook: ',input_file)
 input_file<-normalizePath(input_file);initial_hash<-tools::md5sum(input_file)
 pkgs<-c('readxl','jsonlite','writexl','xml2','zip')
 missing<-pkgs[!vapply(pkgs,requireNamespace,quietly=TRUE,FUN.VALUE=logical(1))]
 if(length(missing))stop('Missing R packages: ',paste(missing,collapse=', '))
 specs<-jsonlite::fromJSON(r"----([{"name":"原始数据_RPKM","columns":["Feature","CKS_1","CKS_2","CKS_3","CKS_4","PES_1","PES_2","PES_3","PES_4","PVCS_1","PVCS_2","PVCS_3","PVCS_4"],"kinds":["string","number","number","number","number","number","number","number","number","number","number","number","number"],"target":"data/分类_门_RPKM.csv"},{"name":"运算数据_相对丰度","columns":["Feature","CKS_1","CKS_2","CKS_3","CKS_4","PES_1","PES_2","PES_3","PES_4","PVCS_1","PVCS_2","PVCS_3","PVCS_4"],"kinds":["string","number","number","number","number","number","number","number","number","number","number","number","number"],"target":"data/分类_门_相对丰度.csv"},{"name":"土壤原始数据","columns":["Treatment","S-ACP","S-β-GC","S-UE","SAHN","SAP","STN","SAK","STP","STK","STC","SDOC","pH","SCd","SACd"],"kinds":["string","number","number","number","number","number","number","number","number","number","number","number","number","number","number"],"target":"data/土壤实测数据.csv"},{"name":"样本映射","columns":["transcriptome","Trait"],"kinds":["string","string"],"target":"data/样本映射关系.csv"},{"name":"配对土壤数据","columns":["Treatment","S-ACP","S-β-GC","S-UE","SAHN","SAP","STN","SAK","STP","STK","STC","SDOC","pH","SCd","SACd","Pot","Sample","Mapping_identifier"],"kinds":["string","number","number","number","number","number","number","number","number","number","number","number","number","number","number","string","string","string"],"target":"data/配对土壤核对表.csv"},{"name":"全部门丰度排名","columns":["Feature","Label","mean_relative_percent","detection_n","selected_top10","rank"],"kinds":["string","string","number","number","boolean","number"],"target":"tables/Phylum_complete_abundance_rank.csv"},{"name":"样本丰度汇总","columns":["Sample","Treatment","relative_sum","RPKM_sum","detected_taxa"],"kinds":["string","string","number","number","number"],"target":"tables/Input_sample_check.csv"},{"name":"Panel_a交集数量","columns":["Code","Count"],"kinds":["string","number"],"target":"tables/Venn_regions.csv"},{"name":"Panel_b坐标","columns":["Sample","Treatment","PCoA1","PCoA2","centroid_distance_bias_adjusted"],"kinds":["string","string","number","number","number"],"target":"tables/PCoA_reused_coordinates.csv"},{"name":"Panel_b群落统计","columns":["Distance","features","R2","P_PERMANOVA","P_PERMDISP_spatial_median","PCoA1_percent","PCoA2_percent"],"kinds":["string","number","number","number","number","number","number"],"target":"tables/PCoA_PERMANOVA_PERMDISP.csv"},{"name":"Panel_c组成","columns":["Feature","Treatment","Relative_percent"],"kinds":["string","string","number"],"target":"tables/Composition_treatment_means.csv"},{"name":"Panel_d完整检验","columns":["rank","Item","Label","mean_relative_percent","prevalence_n","n_per_treatment","H_RPKM","P_RPKM_exact","shown_raw_P_lt_005","CK_mean_RPKM","CK_sd_RPKM","CK_change_vs_CK_percent","PE0.5_mean_RPKM","PE0.5_sd_RPKM","PE0.5_change_vs_CK_percent","PVC5_mean_RPKM","PVC5_sd_RPKM","PVC5_change_vs_CK_percent","F_ANOVA","P_ANOVA","P_Welch_sensitivity","P_residual_Shapiro","P_Brown_Forsythe","largest_to_smallest_SD","min_P_Bonferroni","shown_ANOVA_and_pair_Bonf_lt_005","Welch_sensitivity_P_lt_005"],"kinds":["number","string","string","number","number","number","number","number","boolean","number","number","number","number","number","number","number","number","number","number","number","number","number","number","number","number","boolean","boolean"],"target":"tables/Top10_ANOVA_complete.csv"},{"name":"Panel_d两两比较","columns":["Item","Label","group1","group2","mean_difference_group2_minus_group1","standard_error","t","df","P_raw","P_Bonferroni","comparisons_in_family","Bonferroni_95CI_low","Bonferroni_95CI_high","MSE"],"kinds":["string","string","string","string","number","number","number","number","number","number","number","number","number","number"],"target":"tables/Top10_pairwise_Bonferroni_complete.csv"},{"name":"Panel_d展示门","columns":["rank","Item","Label","mean_relative_percent","prevalence_n","n_per_treatment","H_RPKM","P_RPKM_exact","shown_raw_P_lt_005","CK_mean_RPKM","CK_sd_RPKM","CK_change_vs_CK_percent","PE0.5_mean_RPKM","PE0.5_sd_RPKM","PE0.5_change_vs_CK_percent","PVC5_mean_RPKM","PVC5_sd_RPKM","PVC5_change_vs_CK_percent","F_ANOVA","P_ANOVA","P_Welch_sensitivity","P_residual_Shapiro","P_Brown_Forsythe","largest_to_smallest_SD","min_P_Bonferroni","shown_ANOVA_and_pair_Bonf_lt_005","Welch_sensitivity_P_lt_005"],"kinds":["number","string","string","number","number","number","number","number","boolean","number","number","number","number","number","number","number","number","number","number","number","number","number","number","number","number","boolean","boolean"],"target":"tables/Displayed_differential_phyla.csv"},{"name":"优势门逐样本数据","columns":["Feature","Label","Sample","Treatment","Value","Relative"],"kinds":["string","string","string","string","number","number"],"target":"tables/Top10_sample_abundances.csv"},{"name":"Panel_e完整环境相关","columns":["Unit","Feature","Label","Environment","n","rho","P_raw","rho_treatment_adjusted","P_treatment_adjusted","leave_one_out_min","leave_one_out_max","leave_one_out_same_sign","q_BH_140","q_BH_treatment_adjusted_140","stars"],"kinds":["string","string","string","string","number","number","number","number","number","number","number","number","number","number","string"],"target":"tables/Phylum_environment_Spearman_complete.csv"},{"name":"原始组成图逐样本","columns":["rank","Feature","Label","Sample","Treatment","RPKM","relative_percent"],"kinds":["number","string","string","string","string","number","number"],"target":"tables/出图输入/组合图_前10项_逐样本.csv"},{"name":"原始群落统计","columns":["Level","Distance","features","pseudo_F","R2","P_exact","dispersion_F","dispersion_P_exact","negative_eigenvalue_mass_percent","PCoA1_percent_positive_eigenvalue_sum","PCoA2_percent_positive_eigenvalue_sum","permutation_assignments","q_BH_4community_tests"],"kinds":["string","string","number","number","number","number","number","number","number","number","number","number","number"],"target":"tables/出图输入/群落_PERMANOVA与离散度.csv"},{"name":"离散度检验方法","columns":["method","F","df1","df2","P_residual_permutation","permutations","extreme_permutations","seed","P_exhaustive_allocation_fraction","unique_balanced_allocations","bias_adjust"],"kinds":["string","number","number","number","number","number","number","number","number","number","boolean"],"target":"tables/出图输入/PCoA_PERMDISP方法核查.csv"},{"name":"原始处理差异方法","columns":["Parameter","Value"],"kinds":["string","string"],"target":null},{"name":"Panel对应及方法","columns":["Panel","Content","Input_sheets","Result_sheets","Method"],"kinds":["string","string","string","string","string"],"target":null}])----",simplifyVector=FALSE)
 tables<-setNames(lapply(specs,function(s){
  types<-vapply(s$kinds,function(k)switch(k,number='numeric',boolean='logical',string='text'),character(1))
  d<-as.data.frame(readxl::read_excel(input_file,sheet=s$name,col_types=types,.name_repair='minimal',na=character()),check.names=FALSE)
  stopifnot(identical(names(d),unlist(s$columns)))
  for(j in seq_along(types))if(types[j]=='text')d[[j]][is.na(d[[j]])]<-''
  d
 }),vapply(specs,`[[`,character(1),'name'))
 stage<-tempfile(paste0(r"----(FigS8)----",'_'));dir.create(stage);on.exit(unlink(stage,recursive=TRUE),add=TRUE)
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
 write_alias<-function(sheet,target,rename=FALSE,subset_dtpa=FALSE){
 d<-tables[[sheet]]
 if(rename)names(d)[names(d)=='Item']<-'Feature'
 if(subset_dtpa)d<-d[d$Environment=='DTPA-extractable Cd',,drop=FALSE]
 p<-file.path(stage,target);dir.create(dirname(p),recursive=TRUE,showWarnings=FALSE);write.csv(d,p,row.names=FALSE,na='NA')
}
write_alias('原始数据_RPKM','tables/Phylum_RPKM_input.csv')
write_alias('运算数据_相对丰度','tables/Phylum_relative_input.csv')
write_alias('配对土壤数据','tables/Paired_soil_samples_and_pot_mapping.csv')
write_alias('Panel_b坐标','tables/出图输入/PCoA_phylum_BrayCurtis.csv')
write_alias('Panel_d完整检验','tables/出图输入/组合图_前10项_RPKM完整检验.csv',rename=TRUE)
write_alias('Panel_d两两比较','tables/出图输入/组合图_ANOVA_Bonferroni全部两两比较.csv',rename=TRUE)
write_alias('Panel_d展示门','tables/出图输入/组合图_展示候选.csv',rename=TRUE)
write_alias('Panel_e完整环境相关','tables/Phylum_DTPA_Cd_complete.csv',subset_dtpa=TRUE)
dir.create(file.path(stage,'figure'),showWarnings=FALSE)

 code_files<-list(r"----(code/Phylum_analysis.R)----"=r"----(# Phylum environmental associations; reuse existing group-response statistics.
args<-commandArgs(trailingOnly=TRUE)
sf<-sub('^--file=','',grep('^--file=',commandArgs(),value=TRUE)[1]);code_dir<-dirname(normalizePath(sf))
out<-if(length(args))normalizePath(args[1])else dirname(code_dir)
rroot<-if(length(args)>1)normalizePath(args[2])else dirname(out)
reference<-file.path(out,'tables','出图输入')
td<-file.path(out,'tables');qa<-file.path(out,'QA')
dir.create(td,showWarnings=FALSE,recursive=TRUE);dir.create(qa,showWarnings=FALSE,recursive=TRUE)
suppressPackageStartupMessages(library(jsonlite))
samples<-c(paste0('CKS_',1:4),paste0('PES_',1:4),paste0('PVCS_',1:4));groups<-c('CK','PE0.5','PVC5')
group<-factor(rep(groups,each=4),levels=groups)
paths<-c(file.path(out,'data','分类_门_RPKM.csv'),file.path(out,'data','分类_门_相对丰度.csv'),
 file.path(out,'data','土壤实测数据.csv'),file.path(out,'data','样本映射关系.csv'))
before<-tools::md5sum(paths)
rp<-read.csv(paths[1],check.names=FALSE);rel<-read.csv(paths[2],check.names=FALSE)
stopifnot(identical(rp$Feature,rel$Feature),!anyDuplicated(rp$Feature))
X<-as.matrix(rp[,samples]);P<-as.matrix(rel[,samples]);rownames(X)<-rownames(P)<-rp$Feature
stopifnot(all(is.finite(X)),all(X>=0),all(is.finite(P)),all(P>=0),max(abs(colSums(P)-1))<1e-10,
 max(abs(sweep(X,2,colSums(X),'/')-P))<1e-10)
ix<-order(-rowMeans(P),rownames(P))[1:10];top<-rownames(P)[ix]
read_old<-function(f)read.csv(file.path(reference,f),check.names=FALSE)
old_screen<-read_old('组合图_前10项_RPKM完整检验.csv');stopifnot(identical(top,old_screen$Feature))
top_labels<-old_screen$Label
rank_table<-data.frame(Feature=rownames(P),Label=gsub('_',' ',sub('^p__','',rownames(P))),
 mean_relative_percent=100*rowMeans(P),detection_n=rowSums(P>0),selected_top10=rownames(P)%in%top)
rank_table<-rank_table[order(-rank_table$mean_relative_percent,rank_table$Feature),];rank_table$rank<-seq_len(nrow(rank_table))
write.csv(rank_table,file.path(td,'Phylum_complete_abundance_rank.csv'),row.names=FALSE)
write.csv(data.frame(Sample=samples,Treatment=group,relative_sum=colSums(P),RPKM_sum=colSums(X),detected_taxa=colSums(P>0)),
 file.path(td,'Input_sample_check.csv'),row.names=FALSE)
write.csv(data.frame(Feature=rp$Feature,rp[,samples],check.names=FALSE),file.path(td,'Phylum_RPKM_input.csv'),row.names=FALSE)
write.csv(data.frame(Feature=rel$Feature,rel[,samples],check.names=FALSE),file.path(td,'Phylum_relative_input.csv'),row.names=FALSE)
screen<-old_screen;names(screen)[names(screen)=='Feature']<-'Item'
chosen<-read_old('组合图_展示候选.csv');names(chosen)[names(chosen)=='Feature']<-'Item'
pairs<-read_old('组合图_ANOVA_Bonferroni全部两两比较.csv');names(pairs)[names(pairs)=='Feature']<-'Item'
raw<-do.call(rbind,lapply(seq_along(top),function(k)data.frame(Feature=top[k],Label=top_labels[k],Sample=samples,
 Treatment=group,Value=as.numeric(X[ix[k],]),Relative=as.numeric(P[ix[k],]))))
cm<-rbind(P[ix,,drop=FALSE],Other=1-colSums(P[ix,,drop=FALSE]))
composition<-do.call(rbind,lapply(groups,function(g)data.frame(Feature=rownames(cm),Treatment=g,
 Relative_percent=100*rowMeans(cm[,group==g,drop=FALSE]))))
presence<-sapply(groups,function(g)rowSums(P[,group==g,drop=FALSE]>0)>0)
member<-apply(presence,1,function(z)paste(as.integer(z),collapse=''));region_order<-c('100','010','001','110','101','011','111')
venn_counts<-as.integer(table(factor(member,levels=region_order)))
coords<-read_old('PCoA_phylum_BrayCurtis.csv');stopifnot(identical(coords$Sample,samples),all(coords$Treatment==as.character(group)))
# Verify the full-profile Bray-Curtis geometry against the saved ordination.
D<-as.matrix(dist(t(P),method='manhattan'))/2;H<-diag(12)-1/12;ev<-eigen(-.5*H%*%(D^2)%*%H,symmetric=TRUE)
for(k in 1:2)stopifnot(max(abs(abs(coords[[paste0('PCoA',k)]])-abs(ev$vectors[,k]*sqrt(ev$values[k]))))<1e-9)
z<-read_old('群落_PERMANOVA与离散度.csv');z<-z[z$Level=='phylum' & z$Distance=='Bray–Curtis',]
disp<-read_old('PCoA_PERMDISP方法核查.csv')
community<-data.frame(Distance='Bray-Curtis',features=nrow(P),R2=z$R2,P_PERMANOVA=z$P_exact,
 P_PERMDISP_spatial_median=disp$P_residual_permutation[1],
 PCoA1_percent=z$PCoA1_percent_positive_eigenvalue_sum,PCoA2_percent=z$PCoA2_percent_positive_eigenvalue_sum)
soil<-read.csv(paths[3],check.names=FALSE,fileEncoding='UTF-8-BOM');mp<-read.csv(paths[4],check.names=FALSE,fileEncoding='UTF-8-BOM')
soil$Pot<-paste0(soil$Treatment,'-',ave(seq_len(nrow(soil)),soil$Treatment,FUN=seq_along))
expr_id<-c(paste0('CKS-',1:4),paste0('PE0.5S-',1:4),paste0('PVC5S-',1:4));pot<-mp$Trait[match(expr_id,mp$transcriptome)]
stopifnot(!anyNA(pot),!anyDuplicated(pot));env<-soil[match(pot,soil$Pot),];env$Sample<-samples;env$Mapping_identifier<-expr_id
stopifnot(all(env$Treatment==as.character(group)),!anyNA(env))
genus_env<-read.csv(file.path(out,'data','配对土壤核对表.csv'),check.names=FALSE)
stopifnot(identical(env$Pot,genus_env$Pot),identical(env$Sample,genus_env$Sample))
env_vars<-c('DTPA-extractable Cd','Total Cd','pH','Dissolved organic carbon','Total carbon','Alkali-hydrolysable nitrogen',
 'Total nitrogen','Available phosphorus','Total phosphorus','Available potassium','Total potassium','Urease','β-Glucosidase','Acid phosphatase')
env_cols<-c('SACd','SCd','pH','SDOC','STC','SAHN','STN','SAP','STP','SAK','STK','S-UE','S-β-GC','S-ACP')
E<-as.matrix(env[,env_cols]);colnames(E)<-env_vars
stopifnot(all(is.finite(E)),max(abs(as.matrix(genus_env[,env_cols])-E))<1e-10,all(apply(E,2,sd)>0))
association<-do.call(rbind,lapply(c('Relative abundance','RPKM'),function(unit)do.call(rbind,lapply(seq_along(top),function(k){
 x<-if(unit=='RPKM')X[ix[k],]else P[ix[k],]
 do.call(rbind,lapply(seq_along(env_vars),function(j){
  y<-E[,j];tt<-cor.test(x,y,method='spearman',exact=FALSE)
  rx<-residuals(lm(rank(x)~group));ry<-residuals(lm(rank(y)~group));rr<-cor(rx,ry);df<-8
  pp<-2*pt(-abs(rr)*sqrt(df/max(1e-15,1-rr^2)),df)
  loo<-vapply(1:12,function(i)cor(x[-i],y[-i],method='spearman'),numeric(1))
  data.frame(Unit=unit,Feature=top[k],Label=top_labels[k],Environment=env_vars[j],n=12,rho=unname(tt$estimate),
   P_raw=tt$p.value,rho_treatment_adjusted=rr,P_treatment_adjusted=pp,
   leave_one_out_min=min(loo),leave_one_out_max=max(loo),leave_one_out_same_sign=sum(sign(loo)==sign(tt$estimate)))
 }))}))))
association$q_BH_140<-ave(association$P_raw,association$Unit,FUN=function(p)p.adjust(p,'BH'))
association$q_BH_treatment_adjusted_140<-ave(association$P_treatment_adjusted,association$Unit,FUN=function(p)p.adjust(p,'BH'))
association$stars<-ifelse(association$P_raw<.001,'***',ifelse(association$P_raw<.01,'**',ifelse(association$P_raw<.05,'*','')))
objects<-list(Top10_ANOVA_complete=screen,Displayed_differential_phyla=chosen,Top10_pairwise_Bonferroni_complete=pairs,
 Top10_sample_abundances=raw,Composition_treatment_means=composition,Venn_regions=data.frame(Code=region_order,Count=venn_counts),
 PCoA_reused_coordinates=coords,PCoA_PERMANOVA_PERMDISP=community,Paired_soil_samples_and_pot_mapping=env,
 Phylum_environment_Spearman_complete=association,Phylum_DTPA_Cd_complete=association[association$Environment==env_vars[1],])
for(nm in names(objects))write.csv(objects[[nm]],file.path(td,paste0(nm,'.csv')),row.names=FALSE)
stopifnot(identical(before,tools::md5sum(paths)))
write_json(list(Input_unchanged=TRUE,Input_MD5=as.list(before),Samples=samples,Pot_mapping=as.list(setNames(pot,samples))),
 file.path(qa,'data_check.json'),auto_unbox=TRUE,pretty=TRUE)
capture.output(sessionInfo(),file=file.path(qa,'R_sessionInfo.txt'))
print(association[association$Unit=='Relative abundance'&association$Environment==env_vars[1],c('Label','rho','P_raw','q_BH_140','rho_treatment_adjusted','P_treatment_adjusted')])
)----",r"----(code/phylum_panels.R)----"=r"----(# Reuse established plot construction with existing saved statistics only.
font<-'Arial';axis_pt<-7.5;title_pt<-8.5;annotation_pt<-7;tag_pt<-11
quartzFonts(Arial=quartzFont(c('Arial','Arial-BoldMT','Arial-ItalicMT','Arial-BoldItalicMT')))
colours<-c(CK='#3B5387',PE0.5='#00A087',PVC5='#E64B35')
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
fillcols<-setNames(c(palette[seq_along(top)],'#C7C7C7'),c(top,'Other'))
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
 geom_text(data=bar_sig,aes(x=(x1+x2)/2,y=y+tip*4,label=label),inherit.aes=FALSE,
 family=font,size=annotation_pt/.pt,vjust=0)+
 scale_fill_manual(values=colours,guide='none')+
 scale_x_continuous(breaks=1:3,labels=groups,limits=c(.45,3.55),expand=c(0,0))+
 scale_y_continuous(labels=function(x)formatC(x,format='f',digits=0,big.mark=','),n.breaks=4,expand=expansion(mult=c(0,.14)))+
 facet_wrap(~Item,ncol=1,scales='free_y',axes='all_x',axis.labels='all_x',labeller=as_labeller(bar_labels))+
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
)----",r"----(code/Phylum_plot.R)----"=r"----(# Layout preview: no statistical tests or feature selection are rerun.
args<-commandArgs(trailingOnly=TRUE)
sf<-sub('^--file=','',grep('^--file=',commandArgs(),value=TRUE)[1])
code_dir<-dirname(normalizePath(sf));out<-if(length(args))normalizePath(args[1])else dirname(code_dir)
rroot<-if(length(args)>1)normalizePath(args[2])else normalizePath(dirname(out))
td<-file.path(out,'tables');qa<-file.path(out,'QA');dir.create(qa,showWarnings=FALSE)
suppressPackageStartupMessages({library(ggplot2);library(patchwork);library(grid);library(jsonlite);library(svglite);library(ragg)})
read_saved<-function(f)read.csv(file.path(td,f),check.names=FALSE)
csvs<-list.files(td,full.names=TRUE,pattern='[.]csv$');before<-tools::md5sum(csvs)
groups<-c('CK','PE0.5','PVC5')
screen<-read_saved('Top10_ANOVA_complete.csv');top<-screen$Item;top_labels<-screen$Label
chosen<-read_saved('Displayed_differential_phyla.csv');raw<-read_saved('Top10_sample_abundances.csv')
pairs<-read_saved('Top10_pairwise_Bonferroni_complete.csv');composition<-read_saved('Composition_treatment_means.csv')
venn_counts<-read_saved('Venn_regions.csv')$Count;coords<-read_saved('PCoA_reused_coordinates.csv')
samples<-coords$Sample;zz<-read_saved('PCoA_PERMANOVA_PERMDISP.csv')
R2<-zz$R2;permanova_p<-zz$P_PERMANOVA;dispersion_p<-zz$P_PERMDISP_spatial_median
p1<-zz$PCoA1_percent;p2<-zz$PCoA2_percent
ell<-do.call(rbind,lapply(groups,function(g){v<-as.matrix(coords[coords$Treatment==g,c('PCoA1','PCoA2')]);th<-seq(0,2*pi,length.out=241)
 xy<-sweep(sqrt(qchisq(.95,2))*cbind(cos(th),sin(th))%*%chol(cov(v)),2,colMeans(v),'+')
 data.frame(Treatment=g,PCoA1=xy[,1],PCoA2=xy[,2])}))
association<-read_saved('Phylum_environment_Spearman_complete.csv')
env_vars<-unique(association$Environment)
source(file.path(code_dir,'phylum_panels.R'),local=TRUE)
tagged<-function(p,letter)p+labs(tag=NULL)
# Use a wider rectangular PCoA viewport in the existing upper-left region.
# Coordinate values, ellipse coordinates and all statistics are unchanged.
pc<-pc+coord_cartesian()+scale_y_continuous(expand=expansion(mult=c(.07,.45)),n.breaks=4)
# CSV round-off may place a cumulative percentage infinitesimally above 100.
# Retain every segment; the view is 0–100, with no scale-based data removal.
comp<-comp+scale_x_continuous(breaks=c(0,25,50,75,100),expand=c(0,0))+coord_cartesian(xlim=c(0,100))
# Preserve the matrix's two-line coefficient/star format with clear spacing.
matrix_rel<-matrix_rel+theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))
matrix_rel$layers[[2]]$position<-position_nudge(y=.22)
matrix_rel$layers[[3]]$position<-position_nudge(y=-.25)
matrix_rel$layers[[3]]$aes_params$size<-6.4/.pt
# Retain the existing composition-panel location and left-axis alignment.
comp<-comp+theme(plot.margin=margin(2,4,2,9,'mm'))
bars<-bars+theme(panel.spacing.y=unit(4,'mm'))
plot_objects<-list(a=tagged(venn,'a'),b=tagged(pc,'b'),c=tagged(comp,'c'),d=tagged(bars,'d'),e=tagged(matrix_rel,'e'))
wmm<-220;hmm<-225;upper_height<-123;gap_mm<-2;lower_height<-100
frames<-data.frame(id=c('a','b','c','d','e'),x=c(0,60,0,150,0),y=c(167,167,102,102,0),
 width=c(60,90,150,70,220),height=c(58,58,65,123,100))
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
 if(measure){
 grid.force();vv<-grid.ls(viewports=TRUE,grobs=FALSE,print=FALSE)
 ii<-which(grepl('^panel(?:;[^.]*)?(?:-[0-9]+)*\\.[0-9]+-[0-9]+-[0-9]+-[0-9]+$',vv$name,perl=TRUE))
 pp<-list()
 for(j in ii){tokens<-c(strsplit(vv$vpPath[j],'::',fixed=TRUE)[[1]],vv$name[j]);tokens<-tokens[tokens!='ROOT' & nzchar(tokens)]
 seekViewport(do.call(vpPath,as.list(tokens)));lo<-deviceLoc(unit(0,'npc'),unit(0,'npc'),valueOnly=TRUE);hi<-deviceLoc(unit(1,'npc'),unit(1,'npc'),valueOnly=TRUE);upViewport(0)
 pp[[length(pp)+1]]<-list(id=paste0('plot',length(pp)+1),bbox_pt=72*c(lo$x,lo$y,hi$x,hi$y),viewport=vv$name[j],grid_id=paste0('g',length(pp)+1))}
 pp<-pp[!duplicated(vapply(pp,function(p)paste(round(p$bbox_pt,5),collapse=','),character(1)))]
 pp<-pp[!vapply(pp,function(p)grepl('^panel\\.1-1-1-1$',p$viewport)||grepl('^panel; panel-1-1,',p$viewport),logical(1))]
 right<-pp[vapply(pp,function(p)grepl('^panel-[0-9]+-[0-9]+\\.',p$viewport),logical(1))]
 stopifnot(length(right)==3)
 rows<-list();cols<-list(vapply(right,function(p)p$id,character(1)))
 write_json(list(schema_version=1L,backend='r-grid',figure=list(width_pt=wmm/25.4*72,height_pt=hmm/25.4*72),
 panels=pp,row_groups=rows,column_groups=cols,exemptions=list(),
 note='Compare three repeated phylum-response axes; Venn, rectangular PCoA, composition and full-width heatmap have intentionally different geometry.'),
 file.path(qa,'panel_layout.json'),auto_unbox=TRUE,pretty=TRUE,digits=NA)
 }
}
probe<-tempfile(fileext='.pdf');quartz(type='pdf',file=probe,width=wmm/25.4,height=hmm/25.4,family='Arial');draw_layout(TRUE);dev.off();unlink(probe)
py<-Sys.which('python3')
status<-system2(py,c(shQuote(file.path(code_dir,'audit_panel_alignment.py')),
 shQuote(file.path(qa,'panel_layout.json')),'--json-out',shQuote(file.path(qa,'panel_alignment.json')),'--strict'));stopifnot(status==0L)
svglite(file.path(out,'figure','FigS8.svg'),width=wmm/25.4,height=hmm/25.4);draw_layout();dev.off()
agg_tiff(file.path(out,'figure','FigS8.tiff'),width=wmm,height=hmm,units='mm',res=600,compression='lzw',background='white');draw_layout();dev.off()
stopifnot(identical(before,tools::md5sum(csvs)))
write_json(list(Data_unchanged=TRUE,Statistics_rerun=FALSE,Input_MD5=as.list(before),
 Width_mm=wmm,Height_mm=hmm,Upper_height_mm=upper_height,Lower_height_mm=lower_height,
 Panel_order=c(a='Phylum Venn',b='Phylum PCoA',c='Top10 phylum composition',d='Three differential phyla',e='Phylum–soil matrix'),
 Taxonomic_scope='Phylum-level annotation only',PCoA_physical_axis_ratio='Rectangular plotting region, unequal physical axis scales; coordinate values unchanged',Axis_tick_pt=7.5,Axis_title_pt=8.5,Annotation_pt=7,Panel_letter_pt=11),
 file.path(qa,'layout_contract.json'),auto_unbox=TRUE,pretty=TRUE)

)----",r"----(code/audit_panel_alignment.py)----"=r"----(#!/usr/bin/env python3
"""Audit multi-panel figure alignment from rendered layout geometry.

The core auditor consumes a small backend-neutral JSON manifest. Matplotlib
figures can call :func:`require_matplotlib_panel_alignment` directly after the
final layout draw. R/patchwork figures use ``panel_alignment.R`` to export the
same manifest and then invoke this CLI.

This gate checks the plot-area rectangles that readers perceive as panels. It
does not infer scientific equivalence: comparable row and column groups come
from Matplotlib SubplotSpec metadata, patchwork/gtable metadata, or explicit
groups supplied by the plotting script. Horizontal rows of three or four equal
grid spans must also have equal final physical widths. Asymmetric hero panels,
insets and colorbars must be excluded or exempted with a recorded reason rather
than silently weakening the tolerance.
"""

from __future__ import annotations

import argparse
import html
import json
import math
import os
import re
import tempfile
from collections import defaultdict
from collections.abc import Iterable, Mapping, Sequence
from pathlib import Path
from typing import Any


SCHEMA_VERSION = 1
DEFAULT_TOLERANCE_PT = 1.5
DEFAULT_GUTTER_TOLERANCE_PT = 1.5


class PanelAlignmentError(RuntimeError):
    """Raised when a required in-memory alignment gate does not pass."""


def _atomic_write_text(path: Path, content: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    descriptor, temporary_name = tempfile.mkstemp(
        prefix=f".{path.name}.", suffix=".tmp", dir=path.parent
    )
    temporary = Path(temporary_name)
    try:
        with os.fdopen(descriptor, "w", encoding="utf-8") as handle:
            handle.write(content)
        temporary.replace(path)
    except Exception:
        temporary.unlink(missing_ok=True)
        raise


def _bbox(value: Any) -> tuple[float, float, float, float]:
    if isinstance(value, Mapping):
        values = [value.get(key) for key in ("left", "bottom", "right", "top")]
    else:
        values = list(value) if isinstance(value, Sequence) else []
    if len(values) != 4:
        raise ValueError("panel bbox_pt must contain left, bottom, right and top")
    bbox = tuple(float(item) for item in values)
    if not all(math.isfinite(item) for item in bbox):
        raise ValueError("panel bbox_pt values must be finite")
    if bbox[2] <= bbox[0] or bbox[3] <= bbox[1]:
        raise ValueError("panel bbox_pt must have positive width and height")
    return bbox


def _group_rows(raw: Any, prefix: str) -> list[dict[str, Any]]:
    groups: list[dict[str, Any]] = []
    if raw is None:
        return groups
    if not isinstance(raw, Sequence) or isinstance(raw, (str, bytes)):
        raise ValueError(f"{prefix}_groups must be a list")
    for index, item in enumerate(raw, 1):
        if isinstance(item, Mapping):
            panels = item.get("panels", [])
            group_id = str(item.get("id") or f"{prefix}-{index}")
        else:
            panels = item
            group_id = f"{prefix}-{index}"
        if not isinstance(panels, Sequence) or isinstance(panels, (str, bytes)):
            raise ValueError(f"{prefix} group {group_id} must list panel ids")
        panel_ids = [str(panel) for panel in panels]
        if len(panel_ids) < 2:
            raise ValueError(f"{prefix} group {group_id} needs at least two panels")
        if len(panel_ids) != len(set(panel_ids)):
            raise ValueError(f"{prefix} group {group_id} repeats a panel id")
        groups.append({"id": group_id, "panels": panel_ids})
    return groups


def _inferred_groups(panels: Sequence[dict[str, Any]], orientation: str) -> list[dict[str, Any]]:
    grouped: dict[tuple[Any, ...], list[str]] = defaultdict(list)
    for panel in panels:
        if orientation == "row":
            fields = (panel.get("grid_id"), panel.get("row_start"), panel.get("row_stop"))
        else:
            fields = (panel.get("grid_id"), panel.get("col_start"), panel.get("col_stop"))
        if any(value is None for value in fields):
            continue
        grouped[fields].append(str(panel["id"]))
    output: list[dict[str, Any]] = []
    for index, (_key, panel_ids) in enumerate(grouped.items(), 1):
        if len(panel_ids) >= 2:
            output.append({"id": f"inferred-{orientation}-{index}", "panels": panel_ids})
    return output


def _inferred_boundary_groups(panels: Sequence[dict[str, Any]]) -> list[dict[str, Any]]:
    """Infer shared grid boundaries across panels with unequal spans.

    This covers layouts such as two stacked panels beside one panel that spans
    both rows. Equal-span panels are handled by the ordinary row/column groups,
    so boundary groups are emitted only when at least two different spans meet
    at the same grid boundary.
    """
    specifications = (
        ("top", "row_start", "row_start", "row_stop"),
        ("bottom", "row_stop", "row_start", "row_stop"),
        ("left", "col_start", "col_start", "col_stop"),
        ("right", "col_stop", "col_start", "col_stop"),
    )
    output: list[dict[str, Any]] = []
    for edge, boundary_field, span_start, span_stop in specifications:
        grouped: dict[tuple[Any, Any], list[dict[str, Any]]] = defaultdict(list)
        for panel in panels:
            grid_id = panel.get("grid_id")
            boundary = panel.get(boundary_field)
            start = panel.get(span_start)
            stop = panel.get(span_stop)
            if any(value is None for value in (grid_id, boundary, start, stop)):
                continue
            grouped[(grid_id, boundary)].append(panel)
        edge_index = 0
        for (_grid_id, _boundary), members in grouped.items():
            spans = {(panel.get(span_start), panel.get(span_stop)) for panel in members}
            if len(members) < 2 or len(spans) < 2:
                continue
            edge_index += 1
            output.append(
                {
                    "id": f"inferred-shared-{edge}-{edge_index}",
                    "edge": edge,
                    "panels": [str(panel["id"]) for panel in members],
                }
            )
    return output


def _validated_layout(manifest: Mapping[str, Any]) -> tuple[dict[str, Any] | None, list[str]]:
    errors: list[str] = []
    if manifest.get("schema_version") != SCHEMA_VERSION:
        errors.append(f"schema_version must be {SCHEMA_VERSION}")

    figure = manifest.get("figure")
    if not isinstance(figure, Mapping):
        errors.append("figure must be an object with width_pt and height_pt")
        figure = {}
    try:
        width_pt = float(figure.get("width_pt"))
        height_pt = float(figure.get("height_pt"))
        if not math.isfinite(width_pt) or not math.isfinite(height_pt) or width_pt <= 0 or height_pt <= 0:
            raise ValueError
    except (TypeError, ValueError):
        errors.append("figure width_pt and height_pt must be positive finite numbers")
        width_pt = height_pt = 0.0

    raw_panels = manifest.get("panels")
    panels: list[dict[str, Any]] = []
    if not isinstance(raw_panels, Sequence) or isinstance(raw_panels, (str, bytes)):
        errors.append("panels must be a list")
        raw_panels = []
    for index, raw_panel in enumerate(raw_panels, 1):
        if not isinstance(raw_panel, Mapping):
            errors.append(f"panel {index} must be an object")
            continue
        panel_id = str(raw_panel.get("id") or "").strip()
        if not panel_id:
            errors.append(f"panel {index} is missing id")
            continue
        try:
            bbox = _bbox(raw_panel.get("bbox_pt"))
        except (TypeError, ValueError) as exc:
            errors.append(f"panel {panel_id}: {exc}")
            continue
        if bbox[0] < -0.01 or bbox[1] < -0.01 or bbox[2] > width_pt + 0.01 or bbox[3] > height_pt + 0.01:
            errors.append(f"panel {panel_id} bbox_pt extends beyond the declared figure")
        panel = dict(raw_panel)
        panel["id"] = panel_id
        panel["bbox_pt"] = list(bbox)
        anchor = raw_panel.get("panel_label_anchor_pt")
        if anchor is not None:
            try:
                anchor_values = [float(value) for value in anchor]
                if len(anchor_values) != 2 or not all(math.isfinite(value) for value in anchor_values):
                    raise ValueError
                panel["panel_label_anchor_pt"] = anchor_values
            except (TypeError, ValueError):
                errors.append(f"panel {panel_id} has invalid panel_label_anchor_pt")
        panels.append(panel)

    panel_ids = [panel["id"] for panel in panels]
    if len(panel_ids) != len(set(panel_ids)):
        errors.append("panel ids must be unique")
    if not panels:
        errors.append("at least one panel rectangle is required")

    try:
        row_groups = _group_rows(manifest.get("row_groups"), "row")
        column_groups = _group_rows(manifest.get("column_groups"), "column")
    except ValueError as exc:
        errors.append(str(exc))
        row_groups = column_groups = []
    if not row_groups:
        row_groups = _inferred_groups(panels, "row")
    if not column_groups:
        column_groups = _inferred_groups(panels, "column")
    boundary_groups = _inferred_boundary_groups(panels)

    known_ids = set(panel_ids)
    for group in [*row_groups, *column_groups]:
        missing = [panel for panel in group["panels"] if panel not in known_ids]
        if missing:
            errors.append(f"group {group['id']} references unknown panels: {', '.join(missing)}")

    exemptions: list[dict[str, Any]] = []
    raw_exemptions = manifest.get("exemptions", [])
    if not isinstance(raw_exemptions, Sequence) or isinstance(raw_exemptions, (str, bytes)):
        errors.append("exemptions must be a list")
        raw_exemptions = []
    for index, exemption in enumerate(raw_exemptions, 1):
        if not isinstance(exemption, Mapping):
            errors.append(f"exemption {index} must be an object")
            continue
        exemption_panels = exemption.get("panels", [])
        checks = exemption.get("checks", [])
        reason = str(exemption.get("reason") or "").strip()
        if isinstance(exemption_panels, str):
            exemption_panels = [exemption_panels]
        if isinstance(checks, str):
            checks = [checks]
        if not exemption_panels or any(str(panel) not in known_ids for panel in exemption_panels):
            errors.append(f"exemption {index} must reference known panels")
        allowed_checks = {
            "row",
            "column",
            "panel-width",
            "horizontal-gutter",
            "vertical-gutter",
            "panel-label",
            "all",
        }
        check_names = [str(check) for check in checks]
        if not check_names or any(check not in allowed_checks for check in check_names):
            errors.append(f"exemption {index} has invalid checks")
        if not reason:
            errors.append(f"exemption {index} needs a non-empty reason")
        exemptions.append(
            {"panels": [str(panel) for panel in exemption_panels], "checks": check_names, "reason": reason}
        )

    if errors:
        return None, errors
    return {
        "schema_version": SCHEMA_VERSION,
        "backend": str(manifest.get("backend") or "unknown"),
        "figure": {"width_pt": width_pt, "height_pt": height_pt},
        "panels": panels,
        "row_groups": row_groups,
        "column_groups": column_groups,
        "boundary_groups": boundary_groups,
        "exemptions": exemptions,
    }, []


def _is_exempt(layout: Mapping[str, Any], panel_id: str, check: str) -> bool:
    for exemption in layout.get("exemptions", []):
        if panel_id in exemption["panels"] and (check in exemption["checks"] or "all" in exemption["checks"]):
            return True
    return False


def _metric_spread(values: Mapping[str, float]) -> float:
    return max(values.values()) - min(values.values()) if values else 0.0


def _finding(
    kind: str,
    group: Mapping[str, Any],
    panels: Sequence[str],
    tolerance_pt: float,
    metrics: Mapping[str, Mapping[str, float]],
    message: str,
    severity: str = "FAIL",
) -> dict[str, Any]:
    return {
        "severity": severity,
        "kind": kind,
        "group": group["id"],
        "panels": list(panels),
        "message": message,
        "tolerance_pt": tolerance_pt,
        "metric_spreads_pt": {
            metric: round(_metric_spread(values), 6) for metric, values in metrics.items()
        },
        "values_pt": {
            metric: {panel: round(value, 6) for panel, value in values.items()}
            for metric, values in metrics.items()
        },
    }


def audit_layout_manifest(
    manifest: Mapping[str, Any],
    *,
    tolerance_pt: float = DEFAULT_TOLERANCE_PT,
    gutter_tolerance_pt: float = DEFAULT_GUTTER_TOLERANCE_PT,
    require_panel_labels: bool = False,
) -> dict[str, Any]:
    """Audit a backend-neutral panel-layout manifest."""
    if tolerance_pt < 0 or gutter_tolerance_pt < 0:
        raise ValueError("alignment tolerances must be non-negative")
    layout, errors = _validated_layout(manifest)
    if layout is None:
        return {
            "schema_version": SCHEMA_VERSION,
            "auditable": False,
            "verdict": "NOT AUDITABLE",
            "summary": {"fail": 0, "warn": 0, "comparisons": 0, "exemptions": 0},
            "errors": errors,
            "findings": [],
        }

    if len(layout["panels"]) == 1:
        return {
            "schema_version": SCHEMA_VERSION,
            "applicable": False,
            "auditable": False,
            "verdict": "NOT APPLICABLE",
            "backend": layout["backend"],
            "summary": {"fail": 0, "warn": 0, "comparisons": 0, "exemptions": 0},
            "tolerances": {
                "alignment_pt": tolerance_pt,
                "gutter_pt": gutter_tolerance_pt,
            },
            "errors": [],
            "findings": [],
            "layout": layout,
        }

    panel_map = {panel["id"]: panel for panel in layout["panels"]}
    findings: list[dict[str, Any]] = []
    comparisons = 0

    for orientation, group_key, check_name in (
        ("row", "row_groups", "row"),
        ("column", "column_groups", "column"),
    ):
        for group in layout[group_key]:
            group_panel_ids = list(group["panels"])
            panel_ids = [
                panel_id for panel_id in group_panel_ids if not _is_exempt(layout, panel_id, check_name)
            ]
            if len(panel_ids) < 2:
                continue
            comparisons += 1
            boxes = {panel_id: _bbox(panel_map[panel_id]["bbox_pt"]) for panel_id in panel_ids}
            if orientation == "row":
                metrics = {
                    "top": {panel: box[3] for panel, box in boxes.items()},
                    "bottom": {panel: box[1] for panel, box in boxes.items()},
                    "height": {panel: box[3] - box[1] for panel, box in boxes.items()},
                }
                kind = "row-axes-misalignment"
                message = "Panels intended for one row do not share top/bottom edges and height"
            else:
                metrics = {
                    "left": {panel: box[0] for panel, box in boxes.items()},
                    "right": {panel: box[2] for panel, box in boxes.items()},
                    "width": {panel: box[2] - box[0] for panel, box in boxes.items()},
                }
                kind = "column-axes-misalignment"
                message = "Panels intended for one column do not share left/right edges and width"
            if max(_metric_spread(values) for values in metrics.values()) > tolerance_pt:
                findings.append(_finding(kind, group, panel_ids, tolerance_pt, metrics, message))

            if orientation == "row" and len(group_panel_ids) >= 3:
                width_candidates = [
                    panel_id
                    for panel_id in group_panel_ids
                    if not _is_exempt(layout, panel_id, "row")
                    and not _is_exempt(layout, panel_id, "panel-width")
                ]
                by_column_span: dict[Any, list[str]] = defaultdict(list)
                for panel_id in width_candidates:
                    panel = panel_map[panel_id]
                    try:
                        column_span = float(panel.get("col_stop")) - float(panel.get("col_start"))
                        if not math.isfinite(column_span) or column_span <= 0:
                            raise ValueError
                        span_key: Any = round(column_span, 9)
                    except (TypeError, ValueError):
                        span_key = "unspecified"
                    by_column_span[span_key].append(panel_id)
                for width_index, width_ids in enumerate(by_column_span.values(), 1):
                    if len(width_ids) < 2:
                        continue
                    comparisons += 1
                    width_metrics = {
                        "width": {
                            panel_id: _bbox(panel_map[panel_id]["bbox_pt"])[2]
                            - _bbox(panel_map[panel_id]["bbox_pt"])[0]
                            for panel_id in width_ids
                        }
                    }
                    if _metric_spread(width_metrics["width"]) > tolerance_pt:
                        findings.append(
                            _finding(
                                "horizontal-panel-width-misalignment",
                                {"id": f"{group['id']}-equal-span-{width_index}"},
                                width_ids,
                                tolerance_pt,
                                width_metrics,
                                "Three-or-more-panel row contains unequal final plot-area widths for equal grid spans",
                            )
                        )

            anchors = {
                panel_id: panel_map[panel_id].get("panel_label_anchor_pt") for panel_id in panel_ids
            }
            present_anchors = {panel: anchor for panel, anchor in anchors.items() if anchor is not None}
            missing_labels = [panel for panel, anchor in anchors.items() if anchor is None]
            if require_panel_labels and missing_labels:
                findings.append(
                    {
                        "severity": "WARN",
                        "kind": "panel-label-not-auditable",
                        "group": group["id"],
                        "panels": missing_labels,
                        "message": "Comparable panels are missing detectable top-left panel-label anchors",
                    }
                )
            if len(present_anchors) >= 2:
                label_check = "panel-label"
                label_ids = [
                    panel for panel in panel_ids if panel in present_anchors and not _is_exempt(layout, panel, label_check)
                ]
                if len(label_ids) >= 2:
                    coordinate = 1 if orientation == "row" else 0
                    metric_name = "label-y" if orientation == "row" else "label-x"
                    metrics = {
                        metric_name: {panel: float(present_anchors[panel][coordinate]) for panel in label_ids}
                    }
                    if _metric_spread(metrics[metric_name]) > tolerance_pt:
                        findings.append(
                            _finding(
                                f"{orientation}-panel-label-misalignment",
                                group,
                                label_ids,
                                tolerance_pt,
                                metrics,
                                "Panel-label anchors are not aligned within their comparable group",
                            )
                        )

            gutter_check = "horizontal-gutter" if orientation == "row" else "vertical-gutter"
            gutter_ids = [panel for panel in panel_ids if not _is_exempt(layout, panel, gutter_check)]
            if len(gutter_ids) >= 2:
                if orientation == "row":
                    ordered = sorted(gutter_ids, key=lambda panel: boxes[panel][0])
                    gaps = {
                        f"{first}->{second}": boxes[second][0] - boxes[first][2]
                        for first, second in zip(ordered, ordered[1:])
                    }
                else:
                    ordered = sorted(gutter_ids, key=lambda panel: boxes[panel][3], reverse=True)
                    gaps = {
                        f"{first}->{second}": boxes[first][1] - boxes[second][3]
                        for first, second in zip(ordered, ordered[1:])
                    }
                if gaps and min(gaps.values()) < -tolerance_pt:
                    findings.append(
                        _finding(
                            "panel-axes-overlap",
                            group,
                            gutter_ids,
                            tolerance_pt,
                            {"gutter": gaps},
                            "Panel plot-area rectangles overlap",
                        )
                    )
                elif len(gaps) >= 2 and _metric_spread(gaps) > gutter_tolerance_pt:
                    findings.append(
                        _finding(
                            f"{gutter_check}-misalignment",
                            group,
                            gutter_ids,
                            gutter_tolerance_pt,
                            {"gutter": gaps},
                            "Comparable inter-panel gutters are not uniform",
                        )
                    )

    edge_coordinates = {"left": 0, "bottom": 1, "right": 2, "top": 3}
    for group in layout["boundary_groups"]:
        edge = str(group["edge"])
        check_name = "row" if edge in {"top", "bottom"} else "column"
        panel_ids = [
            panel_id
            for panel_id in group["panels"]
            if not _is_exempt(layout, panel_id, check_name)
        ]
        if len(panel_ids) < 2:
            continue
        comparisons += 1
        coordinate = edge_coordinates[edge]
        metrics = {
            edge: {
                panel_id: _bbox(panel_map[panel_id]["bbox_pt"])[coordinate]
                for panel_id in panel_ids
            }
        }
        if _metric_spread(metrics[edge]) > tolerance_pt:
            findings.append(
                _finding(
                    f"shared-{edge}-edge-misalignment",
                    group,
                    panel_ids,
                    tolerance_pt,
                    metrics,
                    f"Panels meeting the same grid {edge} boundary are not aligned",
                )
            )

        if edge not in {"top", "left"}:
            continue
        anchors = {
            panel_id: panel_map[panel_id].get("panel_label_anchor_pt")
            for panel_id in panel_ids
        }
        missing_labels = [panel_id for panel_id, anchor in anchors.items() if anchor is None]
        if require_panel_labels and missing_labels:
            findings.append(
                {
                    "severity": "WARN",
                    "kind": "panel-label-not-auditable",
                    "group": group["id"],
                    "panels": missing_labels,
                    "message": "Comparable panels are missing detectable top-left panel-label anchors",
                }
            )
        label_ids = [
            panel_id
            for panel_id, anchor in anchors.items()
            if anchor is not None and not _is_exempt(layout, panel_id, "panel-label")
        ]
        if len(label_ids) < 2:
            continue
        label_coordinate = 1 if edge == "top" else 0
        metric_name = "label-y" if edge == "top" else "label-x"
        label_metrics = {
            metric_name: {
                panel_id: float(anchors[panel_id][label_coordinate])
                for panel_id in label_ids
            }
        }
        if _metric_spread(label_metrics[metric_name]) > tolerance_pt:
            findings.append(
                _finding(
                    f"shared-{edge}-panel-label-misalignment",
                    group,
                    label_ids,
                    tolerance_pt,
                    label_metrics,
                    "Panel-label anchors are not aligned at the shared grid boundary",
                )
            )
    fail_count = sum(finding["severity"] == "FAIL" for finding in findings)
    warn_count = sum(finding["severity"] == "WARN" for finding in findings)
    auditable = comparisons > 0
    verdict = "FIX BEFORE DELIVERY" if fail_count else "REVIEW REQUIRED" if warn_count else "PASS"
    if not auditable:
        verdict = "NOT AUDITABLE"
    return {
        "schema_version": SCHEMA_VERSION,
        "applicable": True,
        "auditable": auditable,
        "verdict": verdict,
        "backend": layout["backend"],
        "summary": {
            "fail": fail_count,
            "warn": warn_count,
            "comparisons": comparisons,
            "exemptions": len(layout["exemptions"]),
        },
        "tolerances": {
            "alignment_pt": tolerance_pt,
            "gutter_pt": gutter_tolerance_pt,
        },
        "layout": layout,
        "findings": findings,
        **(
            {"errors": ["No comparable row, column or shared-boundary groups were declared or inferred"]}
            if not auditable
            else {}
        ),
    }


def exit_code(report: Mapping[str, Any], strict: bool = False) -> int:
    if report.get("verdict") == "NOT APPLICABLE":
        return 0
    if not report.get("auditable") or report.get("verdict") == "NOT AUDITABLE":
        return 2
    if int(report.get("summary", {}).get("fail", 0)):
        return 1
    if strict and int(report.get("summary", {}).get("warn", 0)):
        return 1
    return 0


def render_text(report: Mapping[str, Any], strict: bool = False) -> str:
    summary = report.get("summary", {})
    lines = [
        f"Panel alignment: {report.get('verdict', 'UNKNOWN')}",
        f"  comparisons: {summary.get('comparisons', 0)}",
        f"  fail: {summary.get('fail', 0)}",
        f"  warn: {summary.get('warn', 0)}",
        f"  exemptions: {summary.get('exemptions', 0)}",
    ]
    for error in report.get("errors", []):
        lines.append(f"  ERROR: {error}")
    for finding in report.get("findings", []):
        lines.append(
            f"  {finding['severity']} {finding['kind']} [{finding.get('group', 'n/a')}]: "
            f"{finding['message']} ({', '.join(finding.get('panels', []))})"
        )
    if strict and summary.get("warn", 0):
        lines.append("  strict mode: WARN findings block delivery")
    return "\n".join(lines)


def write_json_report(report: Mapping[str, Any], path: str | Path) -> None:
    _atomic_write_text(Path(path), json.dumps(report, indent=2, ensure_ascii=False) + "\n")


def write_overlay_svg(report: Mapping[str, Any], path: str | Path) -> None:
    layout = report.get("layout")
    if not isinstance(layout, Mapping):
        raise ValueError("an auditable layout is required for the diagnostic SVG")
    width = float(layout["figure"]["width_pt"])
    height = float(layout["figure"]["height_pt"])
    failed_panels = {
        panel for finding in report.get("findings", []) if finding.get("severity") == "FAIL"
        for panel in finding.get("panels", [])
    }
    rows = [
        f'<svg xmlns="http://www.w3.org/2000/svg" width="{width}pt" height="{height}pt" viewBox="0 0 {width} {height}">',
        '<rect width="100%" height="100%" fill="white"/>',
    ]
    for panel in layout["panels"]:
        left, bottom, right, top = _bbox(panel["bbox_pt"])
        y = height - top
        color = "#d7191c" if panel["id"] in failed_panels else "#2c7bb6"
        rows.append(
            f'<rect x="{left:.3f}" y="{y:.3f}" width="{right-left:.3f}" height="{top-bottom:.3f}" '
            f'fill="none" stroke="{color}" stroke-width="1.2"/>'
        )
        rows.append(
            f'<text x="{left+2:.3f}" y="{y+10:.3f}" font-family="Arial, sans-serif" font-size="8" '
            f'fill="{color}">{html.escape(str(panel["id"]))}</text>'
        )
    rows.append(
        f'<text x="4" y="{height-5:.3f}" font-family="Arial, sans-serif" font-size="7" fill="#333">'
        f'{report.get("verdict", "UNKNOWN")}: {report.get("summary", {}).get("fail", 0)} fail, '
        f'{report.get("summary", {}).get("warn", 0)} warn</text>'
    )
    rows.append("</svg>")
    _atomic_write_text(Path(path), "\n".join(rows) + "\n")


def _alphabetic_id(index: int) -> str:
    output = ""
    value = index + 1
    while value:
        value, remainder = divmod(value - 1, 26)
        output = chr(ord("a") + remainder) + output
    return output


def _font_is_bold(weight: Any) -> bool:
    if isinstance(weight, (int, float)):
        return float(weight) >= 600
    return str(weight).lower() in {"bold", "semibold", "demibold", "heavy", "black"}


def _matplotlib_panel_label_anchor(ax: Any) -> tuple[str, list[float]] | None:
    for artist in ax.texts:
        label = artist.get_text().strip()
        if not re.fullmatch(r"[a-z]", label) or not _font_is_bold(artist.get_fontweight()):
            continue
        display = artist.get_transform().transform(artist.get_position())
        axes_xy = ax.transAxes.inverted().transform(display)
        if not (-0.35 <= axes_xy[0] <= 0.2 and 0.8 <= axes_xy[1] <= 1.35):
            continue
        inches = ax.figure.dpi_scale_trans.inverted().transform(display)
        return label, [float(inches[0] * 72), float(inches[1] * 72)]
    return None


def matplotlib_layout_manifest(
    fig: Any,
    *,
    axes: Sequence[Any] | None = None,
    panel_ids: Mapping[Any, str] | Sequence[str] | None = None,
    row_groups: Sequence[Any] | None = None,
    column_groups: Sequence[Any] | None = None,
    exclude_axes: Iterable[Any] = (),
    exemptions: Sequence[Mapping[str, Any]] = (),
) -> dict[str, Any]:
    """Measure final Matplotlib axes rectangles in physical points."""
    fig.canvas.draw()
    width_in, height_in = (float(value) for value in fig.get_size_inches())
    width_pt, height_pt = width_in * 72, height_in * 72
    excluded = {id(axis) for axis in exclude_axes}
    candidates = list(axes) if axes is not None else list(fig.axes)
    selected: list[Any] = []
    seen_subplot_cells: set[tuple[int, int, int]] = set()
    for axis in candidates:
        if id(axis) in excluded or not axis.get_visible():
            continue
        axis_label = str(axis.get_label() or "")
        if axis_label.startswith("<colorbar"):
            continue
        try:
            subplot_spec = axis.get_subplotspec()
        except AttributeError:
            subplot_spec = None
        if subplot_spec is not None and panel_ids is None:
            subplot_key = (
                id(subplot_spec.get_gridspec()),
                int(subplot_spec.num1),
                int(subplot_spec.num2),
            )
            if subplot_key in seen_subplot_cells:
                continue
            seen_subplot_cells.add(subplot_key)
        selected.append(axis)

    if isinstance(panel_ids, Mapping):
        ids = [str(panel_ids.get(axis) or "") for axis in selected]
    elif panel_ids is not None:
        ids = [str(value) for value in panel_ids]
        if len(ids) != len(selected):
            raise ValueError("panel_ids length must match the selected Matplotlib axes")
    else:
        ids = []
        for index, axis in enumerate(selected):
            candidate = str(axis.get_gid() or axis.get_label() or "").strip()
            ids.append(candidate if candidate and not candidate.startswith("<") else _alphabetic_id(index))

    grid_ids: dict[int, str] = {}
    panels: list[dict[str, Any]] = []
    for axis, panel_id in zip(selected, ids):
        position = axis.get_position(original=False)
        panel: dict[str, Any] = {
            "id": panel_id,
            "bbox_pt": [
                float(position.x0 * width_pt),
                float(position.y0 * height_pt),
                float(position.x1 * width_pt),
                float(position.y1 * height_pt),
            ],
        }
        try:
            subplot_spec = axis.get_subplotspec()
        except AttributeError:
            subplot_spec = None
        if subplot_spec is not None:
            grid_spec = subplot_spec.get_gridspec()
            grid_key = id(grid_spec)
            if grid_key not in grid_ids:
                grid_ids[grid_key] = f"matplotlib-grid-{len(grid_ids) + 1}"
            panel.update(
                {
                    "grid_id": grid_ids[grid_key],
                    "row_start": int(subplot_spec.rowspan.start),
                    "row_stop": int(subplot_spec.rowspan.stop),
                    "col_start": int(subplot_spec.colspan.start),
                    "col_stop": int(subplot_spec.colspan.stop),
                }
            )
        label_anchor = _matplotlib_panel_label_anchor(axis)
        if label_anchor is not None:
            panel["panel_label"] = label_anchor[0]
            panel["panel_label_anchor_pt"] = label_anchor[1]
        panels.append(panel)

    manifest: dict[str, Any] = {
        "schema_version": SCHEMA_VERSION,
        "backend": "python-matplotlib",
        "figure": {"width_pt": width_pt, "height_pt": height_pt},
        "panels": panels,
        "exemptions": [dict(exemption) for exemption in exemptions],
    }
    if row_groups is not None:
        manifest["row_groups"] = list(row_groups)
    if column_groups is not None:
        manifest["column_groups"] = list(column_groups)
    return manifest


def require_matplotlib_panel_alignment(
    fig: Any,
    *,
    json_out: str | Path | None = None,
    overlay_svg: str | Path | None = None,
    tolerance_pt: float = DEFAULT_TOLERANCE_PT,
    gutter_tolerance_pt: float = DEFAULT_GUTTER_TOLERANCE_PT,
    require_panel_labels: bool = False,
    strict: bool = False,
    **manifest_options: Any,
) -> dict[str, Any]:
    """Measure and block delivery when a Matplotlib multi-panel layout is misaligned."""
    manifest = matplotlib_layout_manifest(fig, **manifest_options)
    report = audit_layout_manifest(
        manifest,
        tolerance_pt=tolerance_pt,
        gutter_tolerance_pt=gutter_tolerance_pt,
        require_panel_labels=require_panel_labels,
    )
    if json_out is not None:
        write_json_report(report, json_out)
    if overlay_svg is not None and report.get("layout"):
        write_overlay_svg(report, overlay_svg)
    code = exit_code(report, strict=strict)
    if code:
        raise PanelAlignmentError(render_text(report, strict=strict))
    return report


def run_self_tests() -> None:
    aligned = {
        "schema_version": 1,
        "backend": "self-test",
        "figure": {"width_pt": 300, "height_pt": 200},
        "panels": [
            {"id": "a", "bbox_pt": [20, 110, 130, 180], "grid_id": "g", "row_start": 0, "row_stop": 1, "col_start": 0, "col_stop": 1},
            {"id": "b", "bbox_pt": [170, 110, 280, 180], "grid_id": "g", "row_start": 0, "row_stop": 1, "col_start": 1, "col_stop": 2},
            {"id": "c", "bbox_pt": [20, 20, 130, 90], "grid_id": "g", "row_start": 1, "row_stop": 2, "col_start": 0, "col_stop": 1},
            {"id": "d", "bbox_pt": [170, 20, 280, 90], "grid_id": "g", "row_start": 1, "row_stop": 2, "col_start": 1, "col_stop": 2},
        ],
    }
    if exit_code(audit_layout_manifest(aligned)) != 0:
        raise AssertionError("aligned self-test layout did not pass")
    shifted = json.loads(json.dumps(aligned))
    shifted["panels"][1]["bbox_pt"][1] -= 5
    if exit_code(audit_layout_manifest(shifted)) != 1:
        raise AssertionError("shifted self-test layout did not fail")
    unequal_widths = {
        "schema_version": 1,
        "backend": "self-test",
        "figure": {"width_pt": 320, "height_pt": 100},
        "panels": [
            {"id": "a", "bbox_pt": [10, 20, 70, 80], "grid_id": "g", "row_start": 0, "row_stop": 1, "col_start": 0, "col_stop": 1},
            {"id": "b", "bbox_pt": [90, 20, 170, 80], "grid_id": "g", "row_start": 0, "row_stop": 1, "col_start": 1, "col_stop": 2},
            {"id": "c", "bbox_pt": [190, 20, 290, 80], "grid_id": "g", "row_start": 0, "row_stop": 1, "col_start": 2, "col_stop": 3},
        ],
    }
    unequal_report = audit_layout_manifest(unequal_widths)
    if exit_code(unequal_report) != 1 or not any(
        finding.get("kind") == "horizontal-panel-width-misalignment"
        for finding in unequal_report.get("findings", [])
    ):
        raise AssertionError("unequal-width self-test row did not fail")


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Audit rendered multi-panel axes alignment from a JSON layout manifest."
    )
    parser.add_argument("layout", nargs="?", help="backend-neutral panel-layout JSON")
    parser.add_argument("--json", action="store_true", help="print the report as JSON")
    parser.add_argument("--json-out", help="write the report atomically as JSON")
    parser.add_argument("--overlay-svg", help="write a QA-only SVG of measured panel rectangles")
    parser.add_argument("--tolerance-pt", type=float, default=DEFAULT_TOLERANCE_PT)
    parser.add_argument("--gutter-tolerance-pt", type=float, default=DEFAULT_GUTTER_TOLERANCE_PT)
    parser.add_argument("--require-panel-labels", action="store_true")
    parser.add_argument("--strict", action="store_true", help="make WARN findings blocking")
    parser.add_argument("--self-test", action="store_true", help="run dependency-free core tests")
    return parser


def main(argv: Sequence[str] | None = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)
    if args.self_test:
        run_self_tests()
        print("Panel alignment self-test: PASS")
        return 0
    if not args.layout:
        parser.error("layout JSON is required unless --self-test is used")
    try:
        manifest = json.loads(Path(args.layout).read_text(encoding="utf-8"))
        report = audit_layout_manifest(
            manifest,
            tolerance_pt=args.tolerance_pt,
            gutter_tolerance_pt=args.gutter_tolerance_pt,
            require_panel_labels=args.require_panel_labels,
        )
    except (OSError, json.JSONDecodeError, TypeError, ValueError) as exc:
        report = {
            "schema_version": SCHEMA_VERSION,
            "auditable": False,
            "verdict": "NOT AUDITABLE",
            "summary": {"fail": 0, "warn": 0, "comparisons": 0, "exemptions": 0},
            "errors": [str(exc)],
            "findings": [],
        }
    if args.json_out:
        write_json_report(report, args.json_out)
    if args.overlay_svg and report.get("layout"):
        write_overlay_svg(report, args.overlay_svg)
    print(json.dumps(report, indent=2, ensure_ascii=False) if args.json else render_text(report, strict=args.strict))
    return exit_code(report, strict=args.strict)


if __name__ == "__main__":
    raise SystemExit(main())
)----")
 for(name in names(code_files)){p<-file.path(stage,name);dir.create(dirname(p),recursive=TRUE,showWarnings=FALSE);writeLines(code_files[[name]],p,useBytes=TRUE)}
 env<-new.env(parent=environment())
 env$commandArgs<-function(trailingOnly=FALSE)if(trailingOnly)paste0('--output=',stage)else base::commandArgs(FALSE)
 env$commandArgs<-function(trailingOnly=FALSE)if(trailingOnly)stage else paste0('--file=',file.path(stage,'code',env$active_script))
 for(script in c('Phylum_analysis.R','Phylum_plot.R')){
  env$active_script<-script;source(file.path(stage,'code',script),local=env)
 }
 
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
  from<-file.path(stage,'figure',paste0(r"----(FigS8)----",'.',ext));stopifnot(file.exists(from))
  if(!file.copy(from,file.path(output_root,paste0(r"----(FigS8)----",'.',ext)),overwrite=TRUE))stop('Could not save figure.')
 }
 if(!'--no-save-data'%in%args)refresh_workbook(tables,file.path(output_root,paste0(r"----(FigS8)----",'_data.xlsx')),input_file)
 message(r"----(FigS8)----",' complete; original observations and analysis definitions preserved.')
}
if ('--typography-only' %in% base::commandArgs(TRUE)) {
  here<-resolve_dir()
  a<-base::commandArgs(TRUE);b<-a[startsWith(a,'--output=')]
  target<-if(length(b))substring(b[1],10)else here
  italic_statistical_P_export(file.path(target,'FigS8.svg'),file.path(target,'FigS8.tiff'))
} else {
  run_figure()
  a<-base::commandArgs(TRUE);b<-a[startsWith(a,'--output=')]
  target<-if(length(b))substring(b[1],10)else resolve_dir()
  italic_statistical_P_export(file.path(target,'FigS8.svg'),file.path(target,'FigS8.tiff'))
}
