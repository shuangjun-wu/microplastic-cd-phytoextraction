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
 input_file<-arg('--input=',file.path(script_dir,paste0(r"----(FigS1)----",'_data.xlsx')))
 output_root<-arg('--output=',script_dir);dir.create(output_root,recursive=TRUE,showWarnings=FALSE)
 if(!file.exists(input_file))stop('Missing workbook: ',input_file)
 input_file<-normalizePath(input_file);initial_hash<-tools::md5sum(input_file)
 pkgs<-c('readxl','jsonlite','writexl','xml2','zip')
 missing<-pkgs[!vapply(pkgs,requireNamespace,quietly=TRUE,FUN.VALUE=logical(1))]
 if(length(missing))stop('Missing R packages: ',paste(missing,collapse=', '))
 specs<-jsonlite::fromJSON(r"----([{"name":"原始拉曼数据","columns":["File","Record","Raman_shift","Intensity"],"kinds":["string","number","number","number"],"target":"raw/raman.csv"},{"name":"原始SEM标定","columns":["Polymer","Image","Header","Pixel_size_um","Strip_height_px","Scale_bar_um","Original_image_sha256"],"kinds":["string","string","string","number","number","number","string"],"target":"merged/SEM_calibration.csv"},{"name":"运算数据_拉曼谱线","columns":["Polymer","Series","Segment","x0_pdf_pt","y0_pdf_pt","x1_pdf_pt","y1_pdf_pt"],"kinds":["string","string","number","number","number","number","number"],"target":"merged/Raman_segments.csv"},{"name":"拉曼刻度","columns":["Polymer","Axis","Label","Position_pdf_pt"],"kinds":["string","string","number","number"],"target":"merged/Raman_ticks.csv"},{"name":"拉曼坐标","columns":["Polymer","left","right","top","bottom"],"kinds":["string","number","number","number","number"],"target":"merged/Raman_bounds.csv"},{"name":"Panel对应表","columns":["Panel","Polymer","Data_folder","SEM_image","SEM_calibration","Raman_segments","Raman_ticks","Raman_bounds","Original_Raman_report","Workbook_data_sheet","Workbook_statistics_sheet"],"kinds":["string","string","string","string","string","string","string","string","string","string","string"],"target":"data/Panel_Data_Index.csv"},{"name":"Panel_a_SEM原图","columns":["Material","Source_file"],"kinds":["string","string"],"target":null},{"name":"Panel_b_SEM原图","columns":["Material","Source_file"],"kinds":["string","string"],"target":null},{"name":"Panel_c_SEM原图","columns":["Material","Source_file"],"kinds":["string","string"],"target":null},{"name":"Panel_d_SEM原图","columns":["Material","Source_file"],"kinds":["string","string"],"target":null},{"name":"Panel_e_SEM原图","columns":["Material","Source_file"],"kinds":["string","string"],"target":null},{"name":"Panel_f_SEM原图","columns":["Material","Source_file"],"kinds":["string","string"],"target":null}])----",simplifyVector=FALSE)
 tables<-setNames(lapply(specs,function(s){
  types<-vapply(s$kinds,function(k)switch(k,number='numeric',boolean='logical',string='text'),character(1))
  d<-as.data.frame(readxl::read_excel(input_file,sheet=s$name,col_types=types,.name_repair='minimal',na=character()),check.names=FALSE)
  stopifnot(identical(names(d),unlist(s$columns)))
  for(j in seq_along(types))if(types[j]=='text')d[[j]][is.na(d[[j]])]<-''
  d
 }),vapply(specs,`[[`,character(1),'name'))
 stage<-tempfile(paste0(r"----(FigS1)----",'_'));dir.create(stage);on.exit(unlink(stage,recursive=TRUE),add=TRUE)
 for(s in specs)if(!is.null(s$target)){
  p<-file.path(stage,s$target);dir.create(dirname(p),recursive=TRUE,showWarnings=FALSE)
  if(grepl('[.]json$',p))writeLines(tables[[s$name]]$JSON[1],p,useBytes=TRUE)else write.csv(tables[[s$name]],p,row.names=FALSE,na='NA')
 }
 asset_specs<-jsonlite::fromJSON(r"----([{"sheet":"Panel_a_SEM原图","md5":"9ed11d37256d4764bd33d9143ed77b12","target":"data/panel_a_PE/1-04.tif"},{"sheet":"Panel_b_SEM原图","md5":"66706bf8ab76b21354a7e17a5ce3781c","target":"data/panel_b_PP/2-08.tif"},{"sheet":"Panel_c_SEM原图","md5":"91d35f708e7404f1013116d3a385ff30","target":"data/panel_c_PS/3-04.tif"},{"sheet":"Panel_d_SEM原图","md5":"194656727475986de239c36bb39f0442","target":"data/panel_d_PA/4-05.tif"},{"sheet":"Panel_e_SEM原图","md5":"7bf74247c269944655fff98cbda4307f","target":"data/panel_e_PET/5-04.tif"},{"sheet":"Panel_f_SEM原图","md5":"acb9a013ca7180b5aa6903afb58e306b","target":"data/panel_f_PVC/6-06.tif"}])----",simplifyVector=FALSE)
 if(length(asset_specs)){
  media<-utils::unzip(input_file,list=TRUE)$Name;media<-media[grepl('^xl/media/',media)&!grepl('/$',media)]
  extract<-file.path(stage,'_images');dir.create(extract);utils::unzip(input_file,files=media,exdir=extract)
  images<-file.path(extract,media);hashes<-unname(tools::md5sum(images))
  for(asset in asset_specs){
   hit<-which(hashes==asset$md5);if(length(hit)!=1)stop('The original image is missing or changed: ',asset$sheet)
   p<-file.path(stage,asset$target);dir.create(dirname(p),recursive=TRUE,showWarnings=FALSE);stopifnot(file.copy(images[hit],p))
  }
 }
 poly<-c('PE','PP','PS','PA','PET','PVC')
 for(i in seq_along(poly)){
  dest<-file.path(stage,'data',paste0('panel_',letters[i],'_',poly[i]));dir.create(dest,recursive=TRUE,showWarnings=FALSE)
  for(s in specs)if(!is.null(s$target)&&startsWith(s$target,'merged/')){
   d<-tables[[s$name]];write.csv(d[d$Polymer==poly[i],],file.path(dest,basename(s$target)),row.names=FALSE,na='NA')
  }
 }
 code_files<-list(r"----(code/FigS1.R)----"=r"----(# Figure S1: native SEM images and original Raman report vector paths.
# Run: Rscript /full/path/to/figS1/code/FigS1.R [--output=/output/root]
args <- commandArgs(trailingOnly=TRUE)
resolve_code_dir <- function() {
 for(fr in rev(sys.frames())) if(!is.null(fr$ofile)&&file.exists(fr$ofile)) return(dirname(normalizePath(fr$ofile)))
 ff <- sub('^--file=','',grep('^--file=',commandArgs(),value=TRUE))
 if(length(ff)&&file.exists(ff[1])) return(dirname(normalizePath(ff[1])))
 if(requireNamespace('rstudioapi',quietly=TRUE)&&rstudioapi::isAvailable()) {
  ff <- rstudioapi::getActiveDocumentContext()$path
  if(nzchar(ff)&&file.exists(ff)) return(dirname(normalizePath(ff)))
 }
 stop('Run the saved script using Rscript or source its full path.')
}
code_dir <- resolve_code_dir();figS1_dir <- dirname(code_dir)
output_arg <- args[startsWith(args,'--output=')]
output_root <- if(length(output_arg)) sub('^--output=','',output_arg[1]) else figS1_dir
out <- file.path(output_root,'figure');dir.create(out,recursive=TRUE,showWarnings=FALSE)
suppressPackageStartupMessages({library(grid);library(magick)})
stopifnot(requireNamespace('ragg',quietly=TRUE),requireNamespace('svglite',quietly=TRUE))
font<-'Arial';ink<-'#263238';W<-240;H<-117
poly<-c('PE','PP','PS','PA','PET','PVC')
folders<-file.path(figS1_dir,'data',paste0('panel_',letters[1:6],'_',poly))
read_panels<-function(name)do.call(rbind,lapply(folders,function(folder)read.csv(file.path(folder,name))))
seg<-read_panels('Raman_segments.csv');ticks<-read_panels('Raman_ticks.csv')
bounds<-read_panels('Raman_bounds.csv');sem<-read_panels('SEM_calibration.csv')
stopifnot(identical(sem$Polymer,poly),all(sem$Pixel_size_um>0),nrow(seg)==23185)
imgs<-lapply(seq_len(nrow(sem)),function(i){
 im<-image_read(file.path(figS1_dir,sem$Image[i]));inf<-image_info(im)
 cropped<-image_crop(im,sprintf('%dx%d+0+0',inf$width,inf$height-sem$Strip_height_px[i]),repage=TRUE)
 list(raster=as.raster(cropped),width=inf$width,height=inf$height-sem$Strip_height_px[i])
})
mmx<-function(x)unit(x,'mm');mmy<-function(y)unit(H-y,'mm')
text_mm<-function(txt,x,y,pt=7,bold=FALSE,col=ink,just='centre',rot=0){
 grid.text(txt,x=mmx(x),y=mmy(y),just=just,rot=rot,gp=gpar(fontfamily=font,fontsize=pt,fontface=if(bold)'bold'else'plain',col=col))
}
layout_records<-list()
record<-function(id){
 a<-deviceLoc(unit(0,'npc'),unit(0,'npc'),valueOnly=TRUE);b<-deviceLoc(unit(1,'npc'),unit(1,'npc'),valueOnly=TRUE)
 layout_records[[length(layout_records)+1]]<<-list(id=id,bbox_pt=72*c(a$x,a$y,b$x,b$y))
}
draw_figure<-function(record_layout=FALSE){
 grid.newpage();pushViewport(viewport(width=mmx(W),height=mmx(H)))
 for(i in seq_along(poly)){
  p<-poly[i];col<-(i-1)%%2;row<-(i-1)%/%2;gx<-4+col*119;gy<-4+row*36.5
  text_mm(letters[i],gx,gy,pt=10,bold=TRUE,just='left');text_mm(p,gx+5.2,gy,pt=9,bold=TRUE,just='left')
  # Native SEM field, cropped only to remove the instrument information strip.
  iw<-37;ih<-iw*imgs[[i]]$height/imgs[[i]]$width;itop<-gy+3.0
  pushViewport(viewport(x=mmx(gx+iw/2),y=mmy(itop+ih/2),width=mmx(iw),height=mmx(ih),clip='on'))
  grid.raster(imgs[[i]]$raster,width=unit(1,'npc'),height=unit(1,'npc'),interpolate=FALSE)
  # Separate scale calibration for each SEM image.
  bar<-100/(sem$Pixel_size_um[i]*imgs[[i]]$width);xr<-.95;yb<-.065
  grid.lines(x=unit(c(xr-bar,xr),'npc'),y=unit(c(yb,yb),'npc'),gp=gpar(col='white',lwd=1.5,lineend='butt'))
  grid.text('100 μm',x=unit(xr-bar/2,'npc'),y=unit(yb+.07,'npc'),gp=gpar(fontfamily=font,fontface='bold',fontsize=6.5,col='white'))
  if(record_layout)record(paste0(letters[i],'_SEM'));popViewport()
  # Original report paths are rendered in the original PDF plot coordinate system.
  rx<-gx+39.0;rtop<-gy+2.5;rw<-74.8;rh<-34.5
  L<-10.5;R<-74.3;T<-.5;B<-28.0;bh<-B-T;bw<-R-L
  bd<-bounds[bounds$Polymer==p,];tk<-ticks[ticks$Polymer==p,]
  fx<-function(x)(x-bd$left)/(bd$right-bd$left)
  fy<-function(y)(bd$bottom-y)/(bd$bottom-bd$top)
  pushViewport(viewport(x=mmx(rx+L+bw/2),y=mmy(rtop+T+bh/2),width=mmx(bw),height=mmx(bh),clip='on'))
  for(ser in c('Reference','Measured')){
   d<-seg[seg$Polymer==p&seg$Series==ser,]
   grid.segments(x0=fx(d$x0_pdf_pt),y0=fy(d$y0_pdf_pt),x1=fx(d$x1_pdf_pt),y1=fy(d$y1_pdf_pt),
    gp=gpar(col=if(ser=='Measured')'#D95746'else'#386CB0',lwd=.60,lineend='round'))
  }
  if(record_layout)record(paste0(letters[i],'_Raman'));popViewport()
  grid.lines(x=mmx(c(rx+L,rx+L,rx+R)),y=mmy(c(rtop+T,rtop+B,rtop+B)),gp=gpar(col='#5B6368',lwd=.5))
  xt<-tk[tk$Axis=='x',];yt<-tk[tk$Axis=='y',]
  for(j in seq_len(nrow(xt))){x<-rx+L+bw*fx(xt$Position_pdf_pt[j]);
   grid.lines(x=mmx(c(x,x)),y=mmy(c(rtop+B,rtop+B+.8)),gp=gpar(col='#5B6368',lwd=.5));text_mm(xt$Label[j],x,rtop+B+2.2,pt=6.6)}
  for(j in seq_len(nrow(yt))){y<-rtop+T+bh*(1-fy(yt$Position_pdf_pt[j]));
   grid.lines(x=mmx(c(rx+L-.7,rx+L)),y=mmy(c(y,y)),gp=gpar(col='#5B6368',lwd=.5));text_mm(yt$Label[j],rx+L-1.5,y,pt=6.6,just='right')}
  if(row==2)text_mm('Raman shift (cm⁻¹)',rx+(L+R)/2,rtop+B+6.3,pt=7.3)
  text_mm('Intensity (CCD counts)',rx+.5,rtop+(T+B)/2,pt=7.3,rot=90)
 }
 # Shared legend above the b panel, outside all plotting areas.
 for(k in 1:2){x<-if(k==1)176 else 205;cc<-if(k==1)'#D95746'else'#386CB0';label<-if(k==1)'Measured'else'Reference'
 grid.lines(x=mmx(c(x,x+4)),y=mmy(rep(2.5,2)),gp=gpar(col=cc,lwd=1.05));text_mm(label,x+5.2,2.5,pt=7,just='left')}
 popViewport()
}
# Only the accepted TIFF and editable SVG are exported.
svglite::svglite(file.path(out,'FigS1.svg'),width=W/25.4,height=H/25.4);draw_figure();dev.off()
ragg::agg_tiff(file.path(out,'FigS1.tiff'),width=W,height=H,units='mm',res=600,compression='lzw',background='white');draw_figure();dev.off()
)----")
 for(name in names(code_files)){p<-file.path(stage,name);dir.create(dirname(p),recursive=TRUE,showWarnings=FALSE);writeLines(code_files[[name]],p,useBytes=TRUE)}
 env<-new.env(parent=environment())
 env$commandArgs<-function(trailingOnly=FALSE)if(trailingOnly)paste0('--output=',stage)else base::commandArgs(FALSE)
 source(file.path(stage,r"----(code/FigS1.R)----"),local=env)
 
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
  from<-file.path(stage,'figure',paste0(r"----(FigS1)----",'.',ext));stopifnot(file.exists(from))
  if(!file.copy(from,file.path(output_root,paste0(r"----(FigS1)----",'.',ext)),overwrite=TRUE))stop('Could not save figure.')
 }
 if(!'--no-save-data'%in%args)refresh_workbook(tables,file.path(output_root,paste0(r"----(FigS1)----",'_data.xlsx')),input_file)
 message(r"----(FigS1)----",' complete; original observations and analysis definitions preserved.')
}
run_figure()
