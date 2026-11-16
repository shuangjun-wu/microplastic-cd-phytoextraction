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
 input_file<-arg('--input=',file.path(script_dir,paste0(r"----(Fig5)----",'_data.xlsx')))
 output_root<-arg('--output=',script_dir);dir.create(output_root,recursive=TRUE,showWarnings=FALSE)
 if(!file.exists(input_file))stop('Missing workbook: ',input_file)
 input_file<-normalizePath(input_file);initial_hash<-tools::md5sum(input_file)
 pkgs<-c('readxl','jsonlite','writexl','xml2','zip')
 missing<-pkgs[!vapply(pkgs,requireNamespace,quietly=TRUE,FUN.VALUE=logical(1))]
 if(length(missing))stop('Missing R packages: ',paste(missing,collapse=', '))
 specs<-jsonlite::fromJSON(r"----([{"name":"Panel_a原始点位","columns":["Organ","Treatment","Source_drawing_index","PDF_x","PDF_y","PC1","PC2"],"kinds":["string","string","number","number","number","number","number"],"target":"data/panel_a_Shoot_PCA/PCA_points.csv"},{"name":"Panel_a椭圆坐标","columns":["Organ","Treatment","Source_drawing_index","Vertex","PC1","PC2"],"kinds":["string","string","number","number","number","number"],"target":"data/panel_a_Shoot_PCA/PCA_ellipse_vertices.csv"},{"name":"Panel_a坐标信息","columns":["JSON"],"kinds":["string"],"target":"data/panel_a_Shoot_PCA/PCA_settings.json"},{"name":"Panel_b原始点位","columns":["Organ","Treatment","Source_drawing_index","PDF_x","PDF_y","PC1","PC2"],"kinds":["string","string","number","number","number","number","number"],"target":"data/panel_b_Root_PCA/PCA_points.csv"},{"name":"Panel_b椭圆坐标","columns":["Organ","Treatment","Source_drawing_index","Vertex","PC1","PC2"],"kinds":["string","string","number","number","number","number"],"target":"data/panel_b_Root_PCA/PCA_ellipse_vertices.csv"},{"name":"Panel_b坐标信息","columns":["JSON"],"kinds":["string"],"target":"data/panel_b_Root_PCA/PCA_settings.json"},{"name":"Panel_c差异基因数量","columns":["Order","Original_contrast","Organ","Treatment_numerator","Treatment_denominator","Up","Down"],"kinds":["number","string","string","string","string","number","number"],"target":"data/panel_c_DEG/DEG_counts.csv"},{"name":"Panel_d_BP完整结果","columns":["GO","Description","Namespace","Annotated_genes","Analysis","Matched_genes","Tested","Reason","P_value","BH_adjusted_P","log2err","ES","NES","Matched_genes_fgsea","Initial_random_samples","Leading_edge_count","P_at_computational_floor","Estimation_status","Organ","Sample_subset","N","Direction","Raw_P_below_005","Significant_BH005","Branch","Result_origin","Status","Displayed_in_panel"],"kinds":["string","string","string","number","string","number","boolean","string","number","number","number","number","number","number","number","number","boolean","string","string","string","number","string","boolean","boolean","string","string","string","boolean"],"target":"data/panel_d_GO_BP/GO_BP_GSEA.csv"},{"name":"Panel_e_CC完整结果","columns":["GO","Description","Namespace","Annotated_genes","Analysis","Matched_genes","Tested","Reason","P_value","BH_adjusted_P","log2err","ES","NES","Matched_genes_fgsea","Initial_random_samples","Leading_edge_count","P_at_computational_floor","Estimation_status","Organ","Sample_subset","N","Direction","Raw_P_below_005","Significant_BH005","Branch","Result_origin","Status","Displayed_in_panel"],"kinds":["string","string","string","number","string","number","boolean","string","number","number","number","number","number","number","number","number","boolean","string","string","string","number","string","boolean","boolean","string","string","string","boolean"],"target":"data/panel_e_GO_CC/GO_CC_GSEA.csv"},{"name":"Panel_f_MF完整结果","columns":["GO","Description","Namespace","Annotated_genes","Analysis","Matched_genes","Tested","Reason","P_value","BH_adjusted_P","log2err","ES","NES","Matched_genes_fgsea","Initial_random_samples","Leading_edge_count","P_at_computational_floor","Estimation_status","Organ","Sample_subset","N","Direction","Raw_P_below_005","Significant_BH005","Branch","Result_origin","Status","Displayed_in_panel"],"kinds":["string","string","string","number","string","number","boolean","string","number","number","number","number","number","number","number","number","boolean","string","string","string","number","string","boolean","boolean","string","string","string","boolean"],"target":"data/panel_f_GO_MF/GO_MF_GSEA.csv"},{"name":"Panel对应表","columns":["Panel","Content","Data_folder","Main_data_file","Categories_or_points","Rows","Source","Workbook_data_sheet","Workbook_statistics_sheet"],"kinds":["string","string","string","string","number","number","string","string","string"],"target":"data/Panel_Data_Index.csv"}])----",simplifyVector=FALSE)
 tables<-setNames(lapply(specs,function(s){
  types<-vapply(s$kinds,function(k)switch(k,number='numeric',boolean='logical',string='text'),character(1))
  d<-as.data.frame(readxl::read_excel(input_file,sheet=s$name,col_types=types,.name_repair='minimal',na=character()),check.names=FALSE)
  stopifnot(identical(names(d),unlist(s$columns)))
  for(j in seq_along(types))if(types[j]=='text')d[[j]][is.na(d[[j]])]<-''
  d
 }),vapply(specs,`[[`,character(1),'name'))
 stage<-tempfile(paste0(r"----(Fig5)----",'_'));dir.create(stage);on.exit(unlink(stage,recursive=TRUE),add=TRUE)
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
 
 code_files<-list(r"----(code/Fig5.py)----"=r"----(import os
os.environ.setdefault('MPLCONFIGDIR','/tmp/fig5_mpl_cache')
import sys,json,hashlib,argparse
from pathlib import Path
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from matplotlib import colors,font_manager
from matplotlib.patches import Rectangle
root=Path(__file__).resolve().parents[1]
parser=argparse.ArgumentParser(description='Render the accepted Fig5 from panel a-f data.')
parser.add_argument('--output',type=Path,default=root,help='Output root; source data stay in the fig5 project.')
out=parser.parse_args().output
(out/'figure').mkdir(parents=True,exist_ok=True)
data=root/'data'
frames=[pd.read_csv(data/folder/filename) for folder,filename in [
 ('panel_d_GO_BP','GO_BP_GSEA.csv'),('panel_e_GO_CC','GO_CC_GSEA.csv'),('panel_f_GO_MF','GO_MF_GSEA.csv')]]
df=pd.concat(frames,ignore_index=True)
df=df[df.Displayed_in_panel.astype(str).str.lower().eq('true')].copy()
assert len(df)==336 and not df.duplicated(['GO','Analysis']).any() and df.N.eq(8).all()
columns=['Shoot__PE0.5_vs_CK','Shoot__PVC5_vs_CK','Root__PE0.5_vs_CK','Root__PVC5_vs_CK']
for f in ['Arial.ttf','Arial Bold.ttf']:font_manager.fontManager.addfont('/System/Library/Fonts/Supplemental/'+f)
font_manager.findfont('Arial',fallback_to_default=False)
plt.rcParams.update({'font.family':'Arial','font.size':8.3,'axes.labelsize':8.3,'xtick.labelsize':8.3,'ytick.labelsize':8.3,'pdf.fonttype':42,'ps.fonttype':42,'svg.fonttype':'none','axes.unicode_minus':False,'savefig.facecolor':'white'})
width_mm,height_mm=241,284;mm=1/25.4
fig=plt.figure(figsize=(width_mm*mm,height_mm*mm),facecolor='white')
gs=fig.add_gridspec(2,2,left=5/width_mm,right=142/width_mm,bottom=12/height_mm,top=194/height_mm,width_ratios=[1,1],height_ratios=[24,23],wspace=77/30,hspace=16/83)
axes=[fig.add_subplot(gs[:,0]),fig.add_subplot(gs[0,1]),fig.add_subplot(gs[1,1])]
# Style-only inheritance: yellow-centred red/cyan palette. NES values remain raw.
cmap=colors.LinearSegmentedColormap.from_list('NES_reference_red_cyan',[(0,'#287FA8'),(.25,'#9CCFD9'),(.5,'#FFF7CC'),(.75,'#F47751'),(1,'#CF2434')],N=256);cmap.set_bad('#D0D0D0')
norm=colors.TwoSlopeNorm(vmin=-3.5,vcenter=0,vmax=3.5)
branch_colors={'BP':'#9D9A35','CC':'#538B79','MF':'#9D7AA9'}
col_colors=['#E97878','#F2B25E','#62BDCD','#688BC1']
def stars(p):return '' if pd.isna(p) or p>=.05 else ('***' if p<.001 else '**' if p<.01 else '*')
records=[];details=[]
for ax,b,title,letter in zip(axes,['BP','CC','MF'],['Biological process','Cellular component','Molecular function'],['d','e','f']):
 z=df[df.Branch.eq(b)].copy();ids=sorted(z.GO.unique());nr=len(ids);assert nr=={'BP':37,'MF':23,'CC':24}[b]
 n=z.pivot(index='GO',columns='Analysis',values='NES').reindex(index=ids,columns=columns);q=z.pivot(index='GO',columns='Analysis',values='BH_adjusted_P').reindex(index=ids,columns=columns)
 names=z.drop_duplicates('GO').set_index('GO').Description.reindex(ids)
 im=ax.imshow(np.ma.masked_invalid(n.values),cmap=cmap,norm=norm,aspect='auto',interpolation='nearest')
 ax.set_xticks([]);ax.set_yticks(range(nr),[v[0].upper()+v[1:] for v in names]);ax.yaxis.tick_right();ax.tick_params(axis='y',length=0,pad=3)
 ax.set_xticks(np.arange(-.5,4,1),minor=True);ax.set_yticks(np.arange(-.5,nr,1),minor=True);ax.grid(which='minor',color='#A4B4B3',linewidth=.28,alpha=.7);ax.tick_params(which='minor',length=0)
 for sp in ax.spines.values():sp.set_visible(False)
 for r,g in enumerate(ids):
  for c,key in enumerate(columns):
   v=n.iloc[r,c];p=q.iloc[r,c]
   if pd.isna(v):text='NA';tc='#4A4A4A'
   else:
    text=stars(p);rgb=cmap(norm(v))[:3];lum=.2126*rgb[0]+.7152*rgb[1]+.0722*rgb[2];tc='white' if lum<.52 else '#222222'
   if text:ax.text(c,r,text,ha='center',va='center',fontsize=7.8,color=tc)
   records.append({'Branch':b,'GO':g,'Description':names.loc[g],'Analysis':key,'NES':v,'BH_adjusted_P':p,'Stars':text if pd.notna(v) else ''})
 # Narrow full-domain bar and four sample-subset color blocks, like the reference.
 box=ax.get_position();left=box.x0;ww=box.width;top=box.y1
 fig.patches.append(Rectangle((left,top+2.2/height_mm),ww,1.4/height_mm,transform=fig.transFigure,facecolor=branch_colors[b],edgecolor='none'))
 for c,col in enumerate(col_colors):fig.patches.append(Rectangle((left+c*ww/4,top+.45/height_mm),ww/4,1.4/height_mm,transform=fig.transFigure,facecolor=col,edgecolor='white',linewidth=.3))
 fig.text(left,top+5/height_mm,title,ha='left',va='bottom',fontsize=9.3,fontweight='bold')
 fig.text(left-3.2/width_mm,top+9/height_mm,letter,ha='left',va='bottom',fontsize=12,fontweight='bold')
 details.append({'Panel':letter,'Branch':b,'Categories':nr,'NA_cells':int(n.isna().sum().sum())})
# One shared legend avoids three copies of identical scales.
lx=200/width_mm
cax=fig.add_axes([200/width_mm,158/height_mm,2.8/width_mm,35/height_mm]);cb=fig.colorbar(im,cax=cax,ticks=[-3,-2,-1,0,1,2,3]);cb.ax.set_title('NES',fontsize=8.5,pad=5);cb.ax.tick_params(labelsize=8,length=2,width=.5,pad=2);cb.outline.set_visible(False)
fig.text(lx,148/height_mm,'GO domain',fontsize=8.5,fontweight='bold',ha='left',va='top')
for i,(b,label) in enumerate([('BP','Biological process'),('CC','Cellular component'),('MF','Molecular function')]):
 yy=141-i*5
 fig.patches.append(Rectangle((200/width_mm,yy/height_mm),2.5/width_mm,2.5/height_mm,transform=fig.transFigure,facecolor=branch_colors[b],edgecolor='none'))
 fig.text(204/width_mm,(yy+1.25)/height_mm,label,fontsize=8,ha='left',va='center')
fig.text(lx,119/height_mm,'Analysis subset',fontsize=8.5,fontweight='bold',ha='left',va='top')
for i,(col,label) in enumerate(zip(col_colors,['Shoot\nCK + PE0.5','Shoot\nCK + PVC5','Root\nCK + PE0.5','Root\nCK + PVC5'])):
 yy=111-i*11
 fig.patches.append(Rectangle((200/width_mm,(yy-1.2)/height_mm),2.5/width_mm,2.5/height_mm,transform=fig.transFigure,facecolor=col,edgecolor='none'))
 fig.text(204/width_mm,(yy+1.2)/height_mm,label,fontsize=8,ha='left',va='top',linespacing=1.15)
fig.text(lx,64/height_mm,'BH-adjusted P',fontsize=8.5,fontweight='bold',ha='left',va='top')
fig.text(lx,57/height_mm,'* < 0.05\n** < 0.01\n*** < 0.001',fontsize=8,ha='left',va='top',linespacing=1.6)
fig.patches.append(Rectangle((200/width_mm,30/height_mm),2.8/width_mm,2.8/height_mm,transform=fig.transFigure,facecolor='#D0D0D0',edgecolor='none'))
fig.text(204/width_mm,31.4/height_mm,'NA',fontsize=8,ha='left',va='center')
# Existing organ-specific PCA and DEG counts; no statistical recomputation.
pca=pd.concat([pd.read_csv(data/folder/'PCA_points.csv') for folder in ['panel_a_Shoot_PCA','panel_b_Root_PCA']],ignore_index=True)
ell=pd.concat([pd.read_csv(data/folder/'PCA_ellipse_vertices.csv') for folder in ['panel_a_Shoot_PCA','panel_b_Root_PCA']],ignore_index=True)
info={v['Organ']:v for v in [json.load(open(data/folder/'PCA_settings.json')) for folder in ['panel_a_Shoot_PCA','panel_b_Root_PCA']]}
deg=pd.read_csv(data/'panel_c_DEG/DEG_counts.csv')
grouppalette={'CK':'#59656B','PE0.5':'#00A087','PVC5':'#E64B35'}
markers={'CK':'o','PE0.5':'^','PVC5':'s'}
topaxes=[fig.add_axes([x/width_mm,218/height_mm,60/width_mm,49/height_mm]) for x in [14,88,171]]
for ax,organ in zip(topaxes[:2],['Shoot','Roots']):
 for g in ['CK','PE0.5','PVC5']:
  z=ell[ell.Organ.eq(organ)&ell.Treatment.eq(g)]
  ax.plot(z.PC1,z.PC2,color=grouppalette[g],lw=.65,zorder=1)
  z=pca[pca.Organ.eq(organ)&pca.Treatment.eq(g)]
  assert len(z)==4
  ax.scatter(z.PC1,z.PC2,c=grouppalette[g],marker=markers[g],s=23,edgecolors='white',linewidths=.3,label=g,zorder=3)
 z=ell[ell.Organ.eq(organ)]
 dx=z.PC1.max()-z.PC1.min();dy=z.PC2.max()-z.PC2.min()
 ax.set_xlim(z.PC1.min()-.08*dx,z.PC1.max()+.08*dx)
 ax.set_ylim(z.PC2.min()-.08*dy,z.PC2.max()+.08*dy)
 ax.axhline(0,color='#B0B0B0',lw=.5,ls=(0,(2,2)),zorder=0)
 ax.axvline(0,color='#B0B0B0',lw=.5,ls=(0,(2,2)),zorder=0)
 ax.set_xlabel('PC1 ('+str(info[organ]['PC1_percent'])+'%)',labelpad=2.5)
 ax.set_ylabel('PC2 ('+str(info[organ]['PC2_percent'])+'%)',labelpad=2.5)
 ax.tick_params(labelsize=8,length=2,width=.6,pad=2)
 from matplotlib.ticker import MaxNLocator
 ax.xaxis.set_major_locator(MaxNLocator(4));ax.yaxis.set_major_locator(MaxNLocator(4))
 for sp in ax.spines.values():sp.set_visible(True);sp.set_linewidth(.6)
leg=topaxes[0].legend(loc='upper right',fontsize=7.6,frameon=False,handlelength=1,handletextpad=.3,borderpad=.1,labelspacing=.25,markerscale=.9)
ax=topaxes[2]
y=np.arange(6)
upcolor='#D5967F';downcolor='#7E9BA6'
ax.barh(y-.18,deg.Up,height=.30,color=upcolor,label='Up',zorder=2)
ax.barh(y+.18,deg.Down,height=.30,color=downcolor,label='Down',zorder=2)
labels=[r.Treatment_numerator+' vs '+r.Treatment_denominator+'\n'+r.Organ.replace('Roots','Root') for r in deg.itertuples()]
ax.set_yticks(y,labels);ax.tick_params(axis='y',length=0,pad=3,labelsize=7.6)
ax.set_ylim(5.65,-.75);ax.set_xlim(0,9000);ax.set_xticks([0,4000,8000]);ax.tick_params(axis='x',length=2,width=.6,pad=2,labelsize=8)
ax.set_xlabel('Number of DEGs',labelpad=2.5)
ax.axhline(2.5,color='#CCCCCC',lw=.5,ls=(0,(2,2)),zorder=0)
for i,r in enumerate(deg.itertuples()):
 for off,v in [(-.18,r.Up),(.18,r.Down)]:ax.text(v+90,i+off,str(v),ha='left',va='center',fontsize=7.6,color='#222222')
for k,sp in ax.spines.items():sp.set_visible(k in ['left','bottom']);sp.set_linewidth(.6)
ax.legend(loc='upper right',ncol=2,fontsize=7.6,frameon=False,handlelength=.9,handletextpad=.35,columnspacing=.7,borderpad=.1)
for ax,title,letter in zip(topaxes,['Shoot','Root','Differentially expressed genes'],['a','b','c']):
 box=ax.get_position()
 fig.text(box.x0,box.y1+2.4/height_mm,title,ha='left',va='bottom',fontsize=9.3,fontweight='bold')
 fig.text(box.x0-3.2/width_mm,box.y1+7/height_mm,letter,ha='left',va='bottom',fontsize=12,fontweight='bold')

fig.canvas.draw()
base=out/'figure/Fig5_PCA_DEG_GO_slim'
fig.savefig(str(base)+'.svg',bbox_inches='tight',pad_inches=.03)
fig.savefig(str(base)+'.tiff',dpi=600,bbox_inches='tight',pad_inches=.03,pil_kwargs={'compression':'tiff_lzw'})
print(base)
)----")
 for(name in names(code_files)){p<-file.path(stage,name);dir.create(dirname(p),recursive=TRUE,showWarnings=FALSE);writeLines(code_files[[name]],p,useBytes=TRUE)}
 env<-new.env(parent=environment())
 env$commandArgs<-function(trailingOnly=FALSE)if(trailingOnly)paste0('--output=',stage)else base::commandArgs(FALSE)
 candidates<-unique(c(arg('--python=',Sys.getenv('FIG5_PYTHON')),
   Sys.which('python3'),file.path(path.expand('~'),'.cache/codex-runtimes/codex-primary-runtime/dependencies/python/bin/python3')))
 cache<-file.path(stage,'mpl_cache');dir.create(cache)
 python_env<-paste0('MPLCONFIGDIR=',shQuote(cache))
 python<-NULL
 for(p in candidates[nzchar(candidates)])if(file.exists(p)){
  status<-suppressWarnings(system2(p,c('-c',shQuote('import numpy,pandas,matplotlib.pyplot,PIL')),env=python_env,stdout=FALSE,stderr=FALSE))
  if(status==0){python<-p;break}
 }
 if(is.null(python))stop('Specify --python= or set FIG5_PYTHON to a Python interpreter with numpy, pandas, matplotlib and Pillow.')
 status<-system2(python,c(shQuote(file.path(stage,'code/Fig5.py')),'--output',shQuote(stage)),env=python_env)
 if(status!=0)stop('Figure 5 rendering failed.')
 
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
  from<-file.path(stage,'figure',paste0(r"----(Fig5_PCA_DEG_GO_slim)----",'.',ext));stopifnot(file.exists(from))
  if(!file.copy(from,file.path(output_root,paste0(r"----(Fig5)----",'.',ext)),overwrite=TRUE))stop('Could not save figure.')
 }
 if(!'--no-save-data'%in%args)refresh_workbook(tables,file.path(output_root,paste0(r"----(Fig5)----",'_data.xlsx')),input_file)
 message(r"----(Fig5)----",' complete; original observations and analysis definitions preserved.')
}
run_figure()
