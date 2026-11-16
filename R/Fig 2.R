# Figure 2: one workbook contains original observations, calculations, statistics and the phenotype photograph.
# Keep Fig2.R and Fig2_data.xlsx in the same folder.
# Run: Rscript /full/path/Fig2.R [--output=/folder] [--input=/data.xlsx]
# By default the workbook is refreshed together with the figure; --no-save-data exports figures only.
resolve_dir<-function(){
 for(fr in rev(sys.frames()))if(!is.null(fr$ofile)&&file.exists(fr$ofile))return(dirname(normalizePath(fr$ofile)))
 ff<-sub('^--file=','',grep('^--file=',commandArgs(),value=TRUE))
 if(length(ff)&&file.exists(ff[1]))return(dirname(normalizePath(ff[1])))
 if(requireNamespace('rstudioapi',quietly=TRUE)&&rstudioapi::isAvailable()){
  ff<-rstudioapi::getActiveDocumentContext()$path
  if(nzchar(ff)&&file.exists(ff))return(dirname(normalizePath(ff)))
 }
 stop('Run the saved Fig2.R using Rscript or source its full path.')
}
fig2_dir<-resolve_dir();args<-commandArgs(trailingOnly=TRUE)
arg<-function(prefix,default){x<-args[startsWith(args,prefix)];if(length(x))substring(x[1],nchar(prefix)+1)else default}
input_file<-arg('--input=',file.path(fig2_dir,'Fig2_data.xlsx'))
output_root<-arg('--output=',fig2_dir);dir.create(output_root,recursive=TRUE,showWarnings=FALSE)
out_dir<-output_root;save_workbook<-!'--no-save-data'%in%args
required<-c('tidyverse','patchwork','magick','multcompView','rstatix','ragg','svglite','readxl')
if(save_workbook)required<-c(required,'writexl','xml2','zip')
missing<-required[!vapply(required,requireNamespace,quietly=TRUE,FUN.VALUE=logical(1))]
if(length(missing))stop('Missing packages: ',paste(missing,collapse=', '))
suppressPackageStartupMessages({library(tidyverse);library(patchwork);library(multcompView)})
if(!file.exists(input_file))stop('Missing Fig2_data.xlsx: ',input_file)
input_file<-normalizePath(input_file);input_hash<-tools::md5sum(input_file)
media<-utils::unzip(input_file,list=TRUE)$Name
media<-media[grepl('^xl/media/.*[.](png|jpg|jpeg)$',media,ignore.case=TRUE)]
if(length(media)!=1L)stop('The workbook must contain exactly one original phenotype photograph.')
photo_dir<-tempfile('fig2_photo_');dir.create(photo_dir)
utils::unzip(input_file,files=media,exdir=photo_dir)
photo_path<-file.path(photo_dir,media)
# Refresh values while preserving the workbook layout and embedded original photograph.
write_workbook_with_photo<-function(sheets,destination,source){
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
 rel_dir<-file.path(newdir,'xl','worksheets','_rels');dir.create(rel_dir,showWarnings=FALSE)
 if(!file.copy(file.path(olddir,'xl','worksheets','_rels','sheet3.xml.rels'),file.path(rel_dir,'sheet3.xml.rels')))stop('Could not preserve the image relationship.')
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
 if(!file.rename(tmp,destination))stop('Could not replace Fig2_data.xlsx. Close it in Excel and retry.')
}
base_family <- "Arial"
if (!"Arial" %in% names(grDevices::pdfFonts())) grDevices::pdfFonts(Arial=grDevices::pdfFonts("ArialMT")[[1]])
col_map<-c(CK="#59636B",PE0.5="#00A087",PVC5="#E64B35")
ord<-names(col_map)
scale_to_percent<-function(x)if(max(x)<=1.2)x*100 else x
pairwise_to_letters <- function(pair_df, mean_df, all_levels, alpha = 0.05) {
  pvec <- pair_df$p.adj
  names(pvec) <- paste(pair_df$group1, pair_df$group2, sep = "-")
  raw_letters <- multcompView::multcompLetters(pvec, threshold = alpha)$Letters

  out <- tibble(
    trt = factor(names(raw_letters), levels = all_levels),
    letters = unname(raw_letters)
  ) %>%
    arrange(trt) %>%
    left_join(
      mean_df %>% transmute(trt = factor(trt, levels = all_levels), mean),
      by = "trt"
    )

  old_letter_order <- out %>%
    arrange(desc(mean)) %>%
    pull(letters) %>%
    paste(collapse = "") %>%
    strsplit("", fixed = TRUE) %>%
    .[[1]]

  old_letter_order <- unique(old_letter_order[old_letter_order != ""])

  if (length(old_letter_order) == 0) {
    return(out %>% select(trt, letters))
  }

  map_tbl <- tibble(old = old_letter_order, new = letters[seq_along(old_letter_order)])

  remap_letters <- function(x) {
    chars <- strsplit(x, "", fixed = TRUE)[[1]]
    chars <- chars[chars != ""]
    new_chars <- map_tbl$new[match(chars, map_tbl$old)]
    paste(sort(unique(new_chars)), collapse = "")
  }

  out %>%
    mutate(letters = vapply(letters, remap_letters, character(1))) %>%
    select(trt, letters)
}

# ---------- fixed stats to match caption ----------
method_map <- c(
  "BDW" = "ANOVA_Tukey",
  "SDW" = "Welch_GamesHowell",
  "RDW" = "ANOVA_Tukey",
  "Berry/Total aboveground" = "ANOVA_Tukey",
  "Root / Total Aboveground" = "ANOVA_Tukey",
  "BCd" = "ANOVA_Tukey",
  "SCd" = "ANOVA_Tukey",
  "RCd" = "ANOVA_Tukey",
  "BTF" = "Welch_GamesHowell",
  "STF" = "ANOVA_Tukey",
  "BECd" = "ANOVA_Tukey",
  "SECd" = "ANOVA_Tukey",
  "AboveExtract" = "Kruskal_Dunn_BH"
)

analyse_trait <- function(data, response, trt = "Treatment", alpha = 0.05) {
  df <- data %>% select(all_of(trt), all_of(response)) %>% drop_na()
  names(df) <- c("trt", "y")
  df$trt <- factor(df$trt, levels = levels(data[[trt]]))

  desc <- df %>%
    group_by(trt) %>%
    summarise(
      mean = mean(y, na.rm = TRUE),
      se = sd(y, na.rm = TRUE) / sqrt(n()),
      n = n(),
      .groups = "drop"
    )

  chosen_method <- method_map[[response]]
  if (is.null(chosen_method)) stop("No test method specified for trait: ", response)

  diag <- tibble(trait = response, method = chosen_method)

  if (chosen_method == "ANOVA_Tukey") {
    fit_aov <- aov(y ~ trt, data = df)
    tukey_df <- as.data.frame(TukeyHSD(fit_aov)$trt) %>%
      rownames_to_column("comparison") %>%
      separate(comparison, into = c("group1", "group2"), sep = "-") %>%
      transmute(group1, group2, p.adj = `p adj`)
    letters_df <- pairwise_to_letters(tukey_df, desc, levels(df$trt), alpha = alpha)
    out <- desc %>% left_join(letters_df, by = "trt") %>% mutate(method = chosen_method)
    return(list(summary = out, diag = diag, pairs = tukey_df))
  }

  if (chosen_method == "Welch_GamesHowell") {
    gh_df <- rstatix::games_howell_test(df, y ~ trt) %>%
      transmute(group1, group2, p.adj)
    letters_df <- pairwise_to_letters(gh_df, desc, levels(df$trt), alpha = alpha)
    out <- desc %>% left_join(letters_df, by = "trt") %>% mutate(method = chosen_method)
    return(list(summary = out, diag = diag, pairs = gh_df))
  }

  if (chosen_method == "Kruskal_Dunn_BH") {
    dunn_df <- rstatix::dunn_test(df, y ~ trt, p.adjust.method = "BH") %>%
      transmute(group1, group2, p.adj)
    letters_df <- pairwise_to_letters(dunn_df, desc, levels(df$trt), alpha = alpha)
    out <- desc %>% left_join(letters_df, by = "trt") %>% mutate(method = chosen_method)
    return(list(summary = out, diag = diag, pairs = dunn_df))
  }

  stop("Unsupported method for trait: ", response)
}


# Source observations are validated and never changed or supplemented.
raw_data<-as.data.frame(readxl::read_excel(input_file,sheet='原始数据',.name_repair='minimal'))
dat<-as_tibble(raw_data)
needed<-c("Treatment",names(method_map)[names(method_map)!="AboveExtract"])
stopifnot(all(needed%in%names(dat)),setequal(unique(dat$Treatment),ord))
for(v in setdiff(needed,"Treatment"))if(!is.numeric(dat[[v]])||any(!is.finite(dat[[v]])))stop("Nonfinite input: ",v)
dat<-dat%>%mutate(Treatment=factor(Treatment,levels=ord),
 `Berry/Total aboveground`=scale_to_percent(`Berry/Total aboveground`),
 `Root / Total Aboveground`=scale_to_percent(`Root / Total Aboveground`),AboveExtract=BECd+SECd)
traits_all<-c("RDW","SDW","BDW","Berry/Total aboveground","Root / Total Aboveground","RCd","SCd","BCd","SECd","BECd","AboveExtract","STF","BTF")
stat_list<-setNames(lapply(traits_all,function(x)analyse_trait(dat,x)),traits_all)

theme_pub<-function(){
 theme_classic(base_size=7,base_family=base_family)+
  theme(text=element_text(colour="#30363A"),
   axis.title.x=element_blank(),axis.title.y=element_text(size=7.2,margin=margin(r=5)),
   axis.text=element_text(size=6.8,colour="#30363A"),
   axis.line=element_line(linewidth=.3,colour="#30363A"),
   axis.ticks=element_line(linewidth=.3,colour="#30363A"),
   axis.ticks.length=grid::unit(1,"mm"),
   strip.background=element_blank(),strip.text=element_text(size=6.8,margin=margin(b=3)),
   panel.spacing=grid::unit(2.5,"mm"),legend.position="none",
   plot.margin=margin(2,3,2,3),
   plot.tag=element_text(size=11,face="bold",colour="black"))
}
make_panel<-function(responses,facet_labels,ylab,bar=FALSE,free_y=TRUE,show_x=FALSE,box=FALSE){
 long<-dat%>%select(Treatment,all_of(responses))%>%
  pivot_longer(-Treatment,names_to="raw_trait",values_to="value")%>%
  mutate(trait=factor(raw_trait,levels=responses,labels=facet_labels),x=as.integer(Treatment))%>%
  group_by(trait,Treatment)%>%mutate(plot_x=x+if(n()==1)0 else seq(-.13,.13,length.out=n()))%>%ungroup()
 ss<-bind_rows(lapply(seq_along(responses),function(i){stat_list[[responses[i]]]$summary%>%mutate(trait=facet_labels[i])}))%>%
  mutate(trait=factor(trait,levels=facet_labels),Treatment=factor(trt,levels=ord),x=as.integer(Treatment))
 spans<-long%>%group_by(trait)%>%summarise(span=max(value)-min(value),.groups="drop")
 extrema<-long%>%group_by(trait,Treatment)%>%summarise(local_max=max(value),.groups="drop")
 ss<-ss%>%left_join(extrema,by=c("trait","Treatment"))%>%left_join(spans,by="trait")%>%
  mutate(label_y=pmax(local_max,mean+se)+.09*ifelse(span>0,span,abs(mean)+1))
 q<-ggplot(long,aes(plot_x,value,colour=Treatment))
 if(bar)q<-q+geom_col(data=ss,aes(x=x,y=mean,fill=Treatment),inherit.aes=FALSE,width=.58,alpha=.80,colour=NA)
 if(box)q<-q+geom_boxplot(data=long,aes(x=x,y=value,group=Treatment,fill=Treatment,colour=Treatment),
  inherit.aes=FALSE,width=.58,outlier.shape=NA,alpha=.55,linewidth=.35)
 q<-q+geom_point(size=1.25,alpha=1)
 if(!box)q<-q+geom_errorbar(data=ss,aes(x=x,ymin=mean-se,ymax=mean+se),inherit.aes=FALSE,width=.17,linewidth=.35,colour="#20272C")+
  geom_errorbar(data=ss,aes(x=x,ymin=mean,ymax=mean),inherit.aes=FALSE,width=.25,linewidth=.5,colour="#20272C")
 q<-q+geom_text(data=ss,aes(x=x,y=label_y,label=letters),inherit.aes=FALSE,family=base_family,size=2.3,vjust=0,colour="#20272C")+
  facet_wrap(~trait,nrow=1,scales=if(free_y)"free_y"else"fixed",axes="all_y",axis.labels="all_y")+
  scale_colour_manual(values=col_map,drop=FALSE)+
  scale_x_continuous(breaks=1:3,labels=ord,limits=c(.55,3.45),expand=expansion(mult=0))+
  scale_y_continuous(expand=expansion(mult=c(.07,.20)),breaks=scales::breaks_pretty(n=4))+
  labs(x=NULL,y=ylab)+theme_pub()
 if(show_x)q<-q+theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1,margin=margin(t=4)))
 else q<-q+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())
 if(bar||box)q<-q+scale_fill_manual(values=col_map,drop=FALSE)
 if(bar)q<-q+expand_limits(y=0)
 q
}
# Read the original image and the documented top-only crop from the workbook.
# No colour adjustment, retouching or inset is applied.
img_original<-magick::image_read(photo_path)
original_info<-magick::image_info(img_original)
photo_spec<-readxl::read_excel(input_file,sheet="Panel_a原图")
top_crop_px<-if("Crop_top_px"%in%names(photo_spec))as.integer(photo_spec$Crop_top_px[1])else 0L
stopifnot(!is.na(top_crop_px),top_crop_px>=0L,top_crop_px<original_info$height[1])
img<-if(top_crop_px>0L)magick::image_crop(img_original,
 geometry=magick::geometry_area(original_info$width[1],original_info$height[1]-top_crop_px,0,top_crop_px),
 repage=TRUE)else img_original
info<-magick::image_info(img)
asp<-info$height[1]/info$width[1]
# Scale panel a into its own upper and side margins, keeping the photo bottom,
# crop and aspect ratio fixed. The shared layout for panels b–f is unchanged.
photo_label_x<-if(top_crop_px>0L)c(.22,.53,.83)else c(.17,.39,.61)
photo_zoom<-if(top_crop_px>0L)1.14 else 1
photo_left<-(1-photo_zoom)/2
p_a<-ggplot()+annotation_custom(grid::rasterGrob(as.raster(img),interpolate=TRUE),
 xmin=photo_left,xmax=photo_left+photo_zoom,ymin=0,ymax=asp*photo_zoom)+
 annotate("text",x=photo_left+photo_zoom*photo_label_x,y=-.028,label=ord,size=2.3,family=base_family,colour="#30363A")+
 coord_fixed(xlim=c(0,1),ylim=c(-.05,asp),expand=FALSE,clip="off")+
 theme_void(base_family=base_family)+
 theme(plot.margin=margin(2,3,2,3))
ylab_dryweight<-expression("Dry weight (g "*plant^{-1}*")")
ylab_cdconc<-expression("Cd concentration (mg "*kg^{-1}*")")
ylab_cdextract<-expression("Cd extraction ("*mu*"g "*pot^{-1}*")")
p_b<-make_panel(c("BDW","SDW","RDW"),c("Berry","Shoot","Root"),ylab_dryweight)
p_c<-make_panel(c("Berry/Total aboveground","Root / Total Aboveground"),c("Berry / aboveground","Root / aboveground"),"Biomass ratio (%)",free_y=FALSE)
p_d<-make_panel(c("BCd","SCd","RCd"),c("Berry","Shoot","Root"),ylab_cdconc,box=TRUE)
p_e<-make_panel(c("BTF","STF"),c("Berry / shoot","Shoot / root"),"Translocation factor",show_x=TRUE,box=TRUE)
p_f<-make_panel(c("BECd","SECd","AboveExtract"),c("Berry","Shoot","Aboveground"),ylab_cdextract,bar=TRUE,show_x=TRUE)
# Narrow individual facets within c/e, keeping their outer rectangles fixed.
p_c<-p_c+theme(panel.spacing=grid::unit(9,"mm"))
p_e<-p_e+theme(panel.spacing=grid::unit(9,"mm"))
# Release the photograph from quantitative y-axis alignment to enlarge it.
# Preserve the cropped photograph's aspect ratio and the full visible plants.
p_a_composite<-patchwork::free(p_a,type="panel",side="l")
final_plot<-wrap_plots(p_a_composite,p_b,p_c,p_d,p_e,p_f,ncol=2,byrow=TRUE,widths=c(1,1.25),heights=c(1.12,1,1.05))+
 plot_annotation(tag_levels="a",theme=theme(plot.tag=element_text(size=11,face="bold",family=base_family)))
final_plot<-final_plot & theme(plot.tag=element_text(size=11,face="bold",family=base_family))
width_mm<-183;height_mm<-145

svg_device<-function(filename,width,height,...)svglite::svglite(filename,width=width,height=height,...)
save_pair<-function(q,stub,w,h,svg=FALSE){
 ggsave(file.path(out_dir,paste0(stub,".tiff")),q,width=w,height=h,units="mm",device=ragg::agg_tiff,dpi=600,compression="lzw",bg="white")
 if(svg){
  ggsave(file.path(out_dir,paste0(stub,".svg")),q,width=w,height=h,units="mm",device=svg_device)
 }
}

# Final rendered facet rectangles are measured in R at the actual export size.
require_patchwork_panel_alignment<-function(q){
 probe<-tempfile(fileext=".pdf");if(capabilities("aqua"))grDevices::quartz(type="pdf",file=probe,width=width_mm/25.4,height=height_mm/25.4,family=base_family)else grDevices::pdf(probe,width=width_mm/25.4,height=height_mm/25.4,family=base_family,useDingbats=FALSE)
 on.exit({grDevices::dev.off();unlink(probe)})
 g<-patchwork::patchworkGrob(q)
 grid::grid.newpage();grid::grid.draw(g);grid::grid.force()
 views<-grid::grid.ls(viewports=TRUE,grobs=FALSE,print=FALSE)$name
 cells<-g$layout[grepl("^panel(?:;|-[0-9])",g$layout$name,perl=TRUE),,drop=FALSE]
 if(nrow(cells)!=6)stop("Cannot resolve six final evidence panels for alignment QA.")
 rects<-lapply(seq_len(nrow(cells)),function(i){
  z<-cells[i,];vp<-views[startsWith(views,paste0(z$name,"."))]
  if(length(vp)!=1)stop("Panel viewport unresolved: ",z$name)
  grid::seekViewport(vp);lo<-grid::deviceLoc(grid::unit(0,"npc"),grid::unit(0,"npc"),valueOnly=TRUE);hi<-grid::deviceLoc(grid::unit(1,"npc"),grid::unit(1,"npc"),valueOnly=TRUE);grid::upViewport(0)
  list(name=z$name,bbox_pt=72*c(lo$x,lo$y,hi$x,hi$y),row=as.integer(z$t),col=as.integer(z$l))
 })
 boxes<-do.call(rbind,lapply(rects,function(x)x$bbox_pt))
 for(ix in list(c(2,4,6),c(3,5)))if(diff(range(boxes[ix,1]))>1.5||diff(range(boxes[ix,3]))>1.5)stop("Panel column alignment exceeds 1.5 pt.")
 for(ix in list(c(3,4),c(5,6)))if(diff(range(boxes[ix,2]))>1.5||diff(range(boxes[ix,4]))>1.5)stop("Panel row alignment exceeds 1.5 pt.")
 manifest<-list(schema_version=1L,backend="r-patchwork",figure=list(width_pt=width_mm/25.4*72,height_pt=height_mm/25.4*72),
   panels=lapply(seq_along(rects),function(i)list(id=base::letters[i],bbox_pt=rects[[i]]$bbox_pt)),
   row_groups=list(c("c","d"),c("e","f")),column_groups=list(c("b","d","f"),c("c","e")),
   exemptions=list(list(panels="a",checks=c("row","column"),reason="Original photograph keeps its native aspect ratio; not a comparable quantitative plot area.")))
 invisible(rects)
}
require_patchwork_panel_alignment(final_plot)
combined_stub<-"Fig2"
save_pair(final_plot,combined_stub,width_mm,height_mm,svg=TRUE)
# All quantitative records and adjusted comparisons are stored in the same workbook.
panel_for_trait<-c(BDW='b',SDW='b',RDW='b','Berry/Total aboveground'='c','Root / Total Aboveground'='c',BCd='d',SCd='d',RCd='d',BTF='e',STF='e',BECd='f',SECd='f',AboveExtract='f')
summary_data<-bind_rows(lapply(names(stat_list),function(x)stat_list[[x]]$summary%>%mutate(trait=x,Panel=unname(panel_for_trait[x]))))
method_data<-bind_rows(lapply(stat_list,function(x)x$diag));method_data$Panel<-unname(panel_for_trait[method_data$trait])
pair_data<-bind_rows(lapply(traits_all,function(x)stat_list[[x]]$pairs%>%mutate(trait=x,method=method_map[[x]],Panel=unname(panel_for_trait[x]))))
summary_data<-summary_data%>%select(Panel,trait,trt,n,mean,se,letters,method)
method_data<-method_data%>%select(Panel,trait,method)
pair_data<-pair_data%>%select(Panel,trait,group1,group2,p.adj,method)
photo_metadata<-data.frame(Source_image=photo_spec$Source_image[1],Width_px=original_info$width[1],Height_px=original_info$height[1],Crop_top_px=top_crop_px)
panel_index<-data.frame(Panel=letters[1:6],Content=c('Plant phenotype','Dry biomass','Biomass allocation ratios','Cd concentrations','Translocation factors','Cd extraction'),Variables=c('Embedded original photograph','BDW; SDW; RDW','Berry/Total aboveground; Root / Total Aboveground','BCd; SCd; RCd','BTF; STF','BECd; SECd; AboveExtract'),Units=c('pixels','g plant^-1','percent','mg kg^-1','dimensionless','microgram pot^-1'))
data_sheets<-list('原始数据'=raw_data,'运算数据'=as.data.frame(dat),'Panel_a原图'=photo_metadata,'Panel_b_f统计'=summary_data,'两两比较'=pair_data,'统计方法'=method_data,'Panel对应表'=panel_index)
stopifnot(identical(input_hash,tools::md5sum(input_file)))
if(save_workbook)write_workbook_with_photo(data_sheets,file.path(output_root,'Fig2_data.xlsx'),input_file)
unlink(photo_dir,recursive=TRUE)
message('Fig2 regenerated; panel a uses the documented crop, and the quantitative analysis is unchanged.')
