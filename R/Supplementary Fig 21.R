# Fig. S21: nitrogen functional genes and soil nitrogen associations.
# Keep this script and FigS21_data.xlsx together.
# Rscript FigS21.R [--output=/path] [--input=/path.xlsx] [--python=/path/python3] [--check-only]
# R dependencies: readxl, jsonlite. Python plotting dependencies: numpy, pandas, matplotlib, Pillow.
# Welch tests are recomputed. Original correlations are read from the workbook.

resolve_dir <- function() {
  for (frame in rev(sys.frames())) if (!is.null(frame$ofile) && file.exists(frame$ofile))
    return(dirname(normalizePath(frame$ofile)))
  f <- sub('^--file=', '', grep('^--file=', commandArgs(), value=TRUE))
  if (length(f) && file.exists(f[1])) return(dirname(normalizePath(f[1])))
  stop('Run the saved script with Rscript or source its full path.')
}

run_figS21 <- function() {
  here <- resolve_dir(); args <- commandArgs(trailingOnly=TRUE)
  arg <- function(key, default) {
    x <- args[startsWith(args,key)]
    if (length(x)) substring(x[1], nchar(key)+1) else default
  }
  input <- normalizePath(arg('--input=',file.path(here,'FigS21_data.xlsx')), mustWork=TRUE)
  output <- arg('--output=',here)
  for (pkg in c('readxl','jsonlite')) if (!requireNamespace(pkg,quietly=TRUE)) stop('Missing R package: ',pkg)
  work <- tempfile('figS21_');dir.create(work)
  on.exit(unlink(work,recursive=TRUE),add=TRUE)
  for (folder in c('data','code','results','QA')) dir.create(file.path(work,folder))
  specs <- jsonlite::fromJSON(r"---([{"sheet": "原始数据", "target": "data/12pot_confirmed_input.csv", "columns": ["PotID", "Treatment", "Metagenome_ID", "Soil_BG", "Soil_DOC_mg_kg", "Soil_UE", "Soil_AN_mg_kg", "Soil_ACP", "Soil_AP_mg_kg", "Soil_AK_mg_kg", "Soil_pH", "Shoot_dry_g"], "kinds": ["text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"], "n": 12}, {"sheet": "KO相对丰度", "target": "data/N_KO_relative.csv", "columns": ["Feature", "CKS_1", "CKS_2", "CKS_3", "CKS_4", "PES_1", "PES_2", "PES_3", "PES_4", "PVCS_1", "PVCS_2", "PVCS_3", "PVCS_4"], "kinds": ["text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"], "n": 62}, {"sheet": "模块丰度", "target": "data/Process_sum_relative.csv", "columns": ["Feature", "CKS_1", "CKS_2", "CKS_3", "CKS_4", "PES_1", "PES_2", "PES_3", "PES_4", "PVCS_1", "PVCS_2", "PVCS_3", "PVCS_4"], "kinds": ["text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"], "n": 6}, {"sheet": "模块与KO对应", "target": "data/Process_gene_definitions.csv", "columns": ["Process", "KO"], "kinds": ["text", "text"], "n": 35}, {"sheet": "KO注释", "target": "data/Gene_annotation_audit.csv", "columns": ["KO", "Name", "Description", "Process", "Literature_core", "Literature_process", "Literature_name", "Detected", "Eligible", "Ambiguous_marker", "Original63", "Reason", "Taxonomic_origin"], "kinds": ["text", "text", "text", "text", "logical", "text", "text", "logical", "logical", "logical", "logical", "text", "text"], "n": 64}, {"sheet": "Panel_b绘图行", "target": "data/Heatmap_display_rows.csv", "columns": ["Group", "KO", "Name", "Shared_DNRA_Denitrification", "Ambiguous_marker"], "kinds": ["text", "text", "text", "logical", "logical"], "n": 35}, {"sheet": "Panel_a模块Welch", "target": "results/Process_treatment_effects.csv", "columns": ["Type", "Feature", "Treatment", "N_CK", "N_treatment", "CK_mean", "Treatment_mean", "CK_SD", "Treatment_SD", "Mean_difference_ppm", "SE_ppm", "Log2FC", "t", "df", "P", "CI_low_ppm", "CI_high_ppm", "Test", "Scale", "P_for_figure"], "kinds": ["text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "text"], "n": 12}, {"sheet": "全部KO_Welch", "target": "results/Gene_treatment_effects.csv", "columns": ["Type", "Feature", "Treatment", "N_CK", "N_treatment", "CK_mean", "Treatment_mean", "CK_SD", "Treatment_SD", "Mean_difference_ppm", "SE_ppm", "Log2FC", "t", "df", "P", "CI_low_ppm", "CI_high_ppm", "Test", "Scale", "P_for_figure"], "kinds": ["text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "text"], "n": 124}, {"sheet": "Panel_b展示KO_Welch", "target": "results/Displayed_KO_Welch_tests.csv", "columns": ["Type", "Feature", "Treatment", "N_CK", "N_treatment", "CK_mean", "Treatment_mean", "CK_SD", "Treatment_SD", "Mean_difference_ppm", "SE_ppm", "Log2FC", "t", "df", "P", "CI_low_ppm", "CI_high_ppm", "Test", "Scale", "P_for_figure"], "kinds": ["text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "text"], "n": 60}, {"sheet": "KO与土壤相关", "target": "results/Gene_soil_associations.csv", "columns": ["Abundance_type", "Adjustment", "KO", "Target", "N", "Rho", "P", "LOO_min", "LOO_max", "LOO_same_sign", "Permutations", "Name", "Description", "Process", "Literature_core", "Literature_process", "Literature_name", "Detected", "Eligible", "Ambiguous_marker", "Original63", "Reason", "Taxonomic_origin"], "kinds": ["text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "logical", "text", "text", "logical", "logical", "logical", "logical", "text", "text"], "n": 496}, {"sheet": "各组丰度摘要", "target": "QA/Abundance_group_diagnostics.csv", "columns": ["Type", "Feature", "Treatment", "N", "Min", "Max", "Mean", "Median", "SD", "CV", "Zero_count", "Largest_fraction_of_sum"], "kinds": ["text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"], "n": 204}])---",simplifyVector=FALSE)
  data <- list()
  for (spec in specs) {
    x <- as.data.frame(readxl::read_excel(input,sheet=spec$sheet,col_types=unlist(spec$kinds),
                       trim_ws=FALSE,.name_repair='minimal'),check.names=FALSE)
    if (!identical(names(x),unlist(spec$columns)) || nrow(x)!=spec$n) stop('Unexpected worksheet structure: ',spec$sheet)
    data[[spec$target]] <- x
    write.csv(x,file.path(work,spec$target),row.names=FALSE,na='',fileEncoding='UTF-8')
  }
  definitions <- data[['data/Process_gene_definitions.csv']]
  display <- data[['data/Heatmap_display_rows.csv']]
  dnra <- unique(definitions$KO[definitions$Process=='DNRA'])
  stopifnot(length(dnra)==9L,nrow(display)==35L,length(unique(display$KO))==30L,
            setequal(dnra,display$KO[display$Group=='DNRA']))
  local({O <- work;eval(parse(text=r"---(d<-read.csv(file.path(O,'data/12pot_confirmed_input.csv'))
stopifnot(nrow(d)==12,anyDuplicated(d$PotID)==0,anyDuplicated(d$Metagenome_ID)==0,all(table(d$Treatment)==4))
out<-list();checks<-list();diagnostics<-list();ii<-0
for(type in c('Gene','Module')){
 fn<-if(type=='Gene')'N_KO_relative.csv'else'Process_sum_relative.csv'
 x<-read.csv(file.path(O,'data',fn),row.names=1,check.names=FALSE)
 stopifnot(all(d$Metagenome_ID%in%colnames(x)),all(is.finite(as.matrix(x))),all(x>=0))
 for(k in rownames(x)){
  for(g in c('CK','PE0.5','PVC5')){
   a<-as.numeric(x[k,d$Metagenome_ID[d$Treatment==g]])*1e6
   diagnostics[[length(diagnostics)+1]]<-data.frame(Type=type,Feature=k,Treatment=g,N=4,Min=min(a),Max=max(a),Mean=mean(a),Median=median(a),SD=sd(a),CV=sd(a)/mean(a),Zero_count=sum(a==0),Largest_fraction_of_sum=max(a)/sum(a))
  }
  a<-as.numeric(x[k,d$Metagenome_ID[d$Treatment=='CK']])*1e6
  for(g in c('PE0.5','PVC5')){
   b<-as.numeric(x[k,d$Metagenome_ID[d$Treatment==g]])*1e6
   z<-t.test(b,a,var.equal=FALSE,paired=FALSE,alternative='two.sided',conf.level=.95)
   delta<-mean(b)-mean(a);va<-var(a)/length(a);vb<-var(b)/length(b);se<-sqrt(va+vb)
   df<-(va+vb)^2/(va^2/(length(a)-1)+vb^2/(length(b)-1))
   tt<-delta/se;pp<-2*pt(-abs(tt),df);ci<-delta+c(-1,1)*qt(.975,df)*se
   stopifnot(abs(unname(z$statistic)-tt)<1e-10,abs(unname(z$parameter)-df)<1e-10,abs(z$p.value-pp)<1e-12,max(abs(z$conf.int-ci))<1e-8)
   ii<-ii+1
   out[[ii]]<-data.frame(Type=type,Feature=k,Treatment=g,N_CK=4,N_treatment=4,CK_mean=mean(a),Treatment_mean=mean(b),CK_SD=sd(a),Treatment_SD=sd(b),Mean_difference_ppm=delta,SE_ppm=se,Log2FC=log2(mean(b)/mean(a)),t=tt,df=df,P=z$p.value,CI_low_ppm=z$conf.int[1],CI_high_ppm=z$conf.int[2],Test='Two-sided unpaired Welch t-test',Scale='Original ppm',P_for_figure='unadjusted')
   checks[[ii]]<-data.frame(Type=type,Feature=k,Treatment=g,P_native=z$p.value,P_manual=pp,P_difference=z$p.value-pp,DF_difference=unname(z$parameter)-df,CI_max_difference=max(abs(z$conf.int-ci)))
  }
 }
}
r<-do.call(rbind,out)
for(type in c('Gene','Module')){
 a<-r[r$Type==type,]
 fn<-if(type=='Gene')'Gene_treatment_effects.csv'else'Process_treatment_effects.csv'
 write.csv(a,file.path(O,'results',fn),row.names=FALSE)
}
write.csv(do.call(rbind,checks),file.path(O,'QA/Manual_Welch_formula_validation.csv'),row.names=FALSE)
write.csv(do.call(rbind,diagnostics),file.path(O,'QA/Abundance_group_diagnostics.csv'),row.names=FALSE)
writeLines(capture.output(sessionInfo()),file.path(O,'QA/R_sessionInfo.txt'))
print(read.csv(file.path(O,'results/Process_treatment_effects.csv'))[,c('Feature','Treatment','Log2FC','P')],row.names=FALSE,digits=5)
)---"), envir=environment())})
  check <- function(new, old) {
    if (!identical(names(new),names(old)) || nrow(new)!=nrow(old)) stop('Unexpected result shape.')
    for (col in names(new)) {
      a <- new[[col]]; b <- old[[col]]
      if (is.numeric(a)) {
        if (!identical(is.na(a),is.na(b)) || any(abs(a-b)>1e-12+1e-12*abs(b),na.rm=TRUE))
          stop('Recomputed values differ from saved results: ',col)
      } else if (!identical(a,b)) stop('Recomputed labels differ: ',col)
    }
  }
  result <- list()
  for (p in c('results/Process_treatment_effects.csv','results/Gene_treatment_effects.csv')) {
    x <- read.csv(file.path(work,p),check.names=FALSE)
    check(x,data[[p]]);result[[p]] <- x
  }
  genes <- result[['results/Gene_treatment_effects.csv']]
  shown <- data[['results/Displayed_KO_Welch_tests.csv']]
  key <- function(x) paste(x$Feature,x$Treatment,sep=':')
  ii <- match(key(shown),key(genes))
  stopifnot(nrow(genes)==124L,nrow(result[['results/Process_treatment_effects.csv']])==12L,
            nrow(shown)==60L,!anyNA(ii),setequal(unique(shown$Feature),unique(display$KO)))
  check(genes[ii,],shown)
  message('Verified 136 Welch comparisons; original correlation results retained.')
  if ('--check-only' %in% args) return(invisible(TRUE))
  candidates <- unique(c(arg('--python=',Sys.getenv('FIGS10_PYTHON')), Sys.which('python3'),
                file.path(path.expand('~'),'.cache/codex-runtimes/codex-primary-runtime/dependencies/python/bin/python3')))
  candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
  cache <- file.path(work,'mpl_cache');dir.create(cache)
  env <- paste0('MPLCONFIGDIR=',shQuote(cache))
  python <- NULL
  for (p in candidates) {
    status <- suppressWarnings(system2(p,c('-c',shQuote('import numpy,pandas,matplotlib.pyplot,PIL')),env=env,stdout=FALSE,stderr=FALSE))
    if (identical(status,0L)) {python <- p;break}
  }
  if (is.null(python)) stop('Specify --python= with numpy, pandas, matplotlib and Pillow installed.')
  plot <- file.path(work,'code/plot.py');writeLines(r"---(from pathlib import Path
import json
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from matplotlib.colors import TwoSlopeNorm
from matplotlib.cm import ScalarMappable
from matplotlib.patches import Rectangle
from matplotlib.lines import Line2D

O=Path(__file__).resolve().parent.parent
export_checks=[]
plt.rcParams.update({'font.family':'Arial','font.size':7,'axes.titlesize':8,'axes.labelsize':7.5,
 'axes.linewidth':.6,'xtick.labelsize':6.5,'ytick.labelsize':6.5,'pdf.fonttype':42,'ps.fonttype':42,
 'svg.fonttype':'none','savefig.facecolor':'white','axes.spines.top':False,'axes.spines.right':False,
 'mathtext.fontset':'custom','mathtext.rm':'Arial','mathtext.it':'Arial:italic','mathtext.bf':'Arial:bold'})
cols={'CK':'#8C969D','PE0.5':'#009E73','PVC5':'#E69F00'}
order=['CK','PE0.5','PVC5']; rng=np.random.default_rng(20261102)
d=pd.read_csv(O/'data/12pot_confirmed_input.csv')
audit=pd.read_csv(O/'data/Gene_annotation_audit.csv').set_index('KO')
r=pd.read_csv(O/'results/Gene_soil_associations.csv')
t=pd.read_csv(O/'results/Gene_treatment_effects.csv').set_index(['Feature','Treatment'])
rel=pd.read_csv(O/'data/N_KO_relative.csv',index_col=0)
proc=pd.read_csv(O/'data/Process_sum_relative.csv',index_col=0)
pt=pd.read_csv(O/'results/Process_treatment_effects.csv')
pmap={'Nitrogen fixation':'Nitrogen_fixation_abundance','Ammonification':'Urea_hydrolysis_abundance','Ammonia assimilation':'Ammonia_assimilation_abundance','Nitrification':'Nitrification_related_abundance','Denitrification':'Denitrification_related_abundance','DNRA':'DNRA_related_abundance'}
pt['Feature']=pt.Feature.replace({v:k for k,v in pmap.items()})
pt=pt.set_index(['Feature','Treatment'])
defs=pd.read_csv(O/'data/Process_gene_definitions.csv')
shared=set(defs[defs.Process.eq('DNRA')].KO)&set(defs[defs.Process.eq('Denitrification')].KO)

display=pd.read_csv(O/'data/Heatmap_display_rows.csv')
literal=display.drop_duplicates('KO').set_index('KO').Name.to_dict()
literal['K00262']='gdhA (NADP)'
blockcolors=['#D5ECE5','#ECE4F0','#F1E4C4','#DCE8F4','#F3DEDC']
def mark(p):return '***' if p<.001 else '**' if p<.01 else '*' if p<.05 else ''
def save(fig,name):
 out=O if name=='FigS21' else O/'figures'
 fig.canvas.draw();renderer=fig.canvas.get_renderer();fw,fh=fig.canvas.get_width_height()
 outside=[]
 for text in fig.findobj(matplotlib.text.Text):
  if not text.get_visible() or not text.get_text():continue
  bb=text.get_window_extent(renderer)
  if bb.x0 < -2 or bb.y0 < -2 or bb.x1>fw+2 or bb.y1>fh+2:outside.append(text.get_text())
 export_checks.append(dict(Figure=name,Outside_canvas_text=outside,Width_mm=fig.get_figwidth()*25.4,Height_mm=fig.get_figheight()*25.4))
 for ext in ['svg']:
  fig.savefig(out/f'{name}.{ext}',dpi=300)
 fig.savefig(out/f'{name}.tiff',dpi=600,pil_kwargs={'compression':'tiff_lzw'})
 plt.close(fig)
def boxes(ax,feature,title):
 vals=[proc.loc[feature,d.loc[d.Treatment.eq(g),'Metagenome_ID']].values*1e6 for g in order]
 bp=ax.boxplot(vals,positions=[0,1,2],widths=.48,patch_artist=True,showfliers=False,
  medianprops={'color':'#333','linewidth':.8},boxprops={'linewidth':.6},whiskerprops={'linewidth':.6},capprops={'linewidth':.6})
 for b,g in zip(bp['boxes'],order):b.set(facecolor=cols[g],alpha=.45)
 for i,(g,v) in enumerate(zip(order,vals)):
  ax.scatter(i+np.linspace(-.11,.11,4),v,s=12,c=cols[g],edgecolor='white',linewidth=.35,zorder=3)
 ax.set_xticks([0,1,2],order);ax.set_xlim(-.55,2.55)
 ax.set_title(title,pad=4,fontweight='bold',bbox=dict(facecolor='#EEEEEE',edgecolor='none',pad=2.5))
 ax.tick_params(length=2.5,pad=2);ax.set_ylabel('KO abundance (ppm)')
 rr=pt.loc[feature]
 ax.text(.98,.97,r'Welch $P$: '+', '.join(f'{rr.loc[g,"P"]:.3f}' for g in order[1:]),ha='right',va='top',
  transform=ax.transAxes,fontsize=6)
 ax.margins(y=.28)

def matrix(fig,rect,genes,groupnames,namefontsize=6.7):
 """Matched rows: Welch treatment effects, unchanged overall and adjusted correlations."""
 x,y,w,h=rect; rowpositions=[]; boundaries=[]; cursor=0
 for g,kk in groupnames:
  start=cursor; rowpositions.extend((k,cursor+i) for i,k in enumerate(kk));cursor+=len(kk)
  boundaries.append((g,start,cursor));cursor+=.9
 n=cursor-.9; gx=dict(rowpositions)
 axl=fig.add_axes([x,y,w*.26,h]); axc=fig.add_axes([x+w*.29,y,w*.29,h]); axa=fig.add_axes([x+w*.62,y,w*.29,h])
 axes=[axl,axc,axa]
 fclimit=max(.5,float(np.ceil(max(abs(t.loc[(k,g),'Log2FC']) for k in genes for g in order[1:])*2)/2))
 fn=TwoSlopeNorm(vmin=-fclimit,vcenter=0,vmax=fclimit);rn=TwoSlopeNorm(vmin=-1,vcenter=0,vmax=1)
 mats=[]
 for ax in axes:
  ax.set_ylim(n-.5,-.5);ax.set_xlim(-.5,1.5);ax.set_yticks([])
  for sp in ax.spines.values():sp.set_visible(False)
  ax.tick_params(axis='x',length=0,pad=3);ax.xaxis.tick_top()
 axl.set_xticks([0,1],['PE0.5 / CK','PVC5 / CK']);axc.set_xticks([0,1],['Urease','AN']);axa.set_xticks([0,1],['Urease','AN'])
 axl.set_title('Treatment change\n'+r'$\log_2$(mean ratio)',pad=22,fontweight='bold')
 axc.set_title('Overall association\nSpearman ρ',pad=22,fontweight='bold')
 axa.set_title('Treatment-adjusted\nRank residual correlation',pad=22,fontweight='bold')
 for g,lo,hi in boundaries:
  for ax in axes:ax.axhspan(lo-.5,hi-.5,color='#F7F7F7',zorder=-2)
  mid=(lo+hi-1)/2
  # separate strip and horizontal caption between functional sections
  axl.text(-.07,1-(mid+.5)/n,g,ha='right',va='center',transform=axl.transAxes,fontsize=namefontsize,fontweight='bold')
 for k,yy in rowpositions:
  for j,g in enumerate(order[1:]):
   v=t.loc[(k,g)]; axl.add_patch(Rectangle((j-.5,yy-.5),1,1,facecolor=plt.cm.RdBu_r(fn(v.Log2FC)),edgecolor='white',linewidth=.6))
   axl.text(j,yy,f'{v.Log2FC:+.2f}'+mark(v.P),ha='center',va='center',fontsize=6.2,color='white' if abs(v.Log2FC)>.75*fclimit else '#222')
  for ax,adj in [(axc,'Overall'),(axa,'Treatment-adjusted')]:
   rr=r[(r.Abundance_type=='Relative')&(r.Adjustment==adj)&r.KO.eq(k)].set_index('Target')
   for j,target in enumerate(['Soil_UE','Soil_AN_mg_kg']):
    v=rr.loc[target];ax.add_patch(Rectangle((j-.5,yy-.5),1,1,facecolor=plt.cm.RdBu_r(rn(v.Rho)),edgecolor='white',linewidth=.6))
    ax.text(j,yy,f'{v.Rho:+.2f}'+mark(v.P),ha='center',va='center',fontsize=6.2,color='white' if abs(v.Rho)>.75 else '#222')
  label=literal.get(k,audit.loc[k,'Name'])
  label=label.replace('E1.4.1.4, gdhA','gdhA').replace('GLUD1_2, gdhA','gdhA').replace('amt, AMT, MEP','amt').replace('NRT2, narK, nrtP, nasA','narK / nrtP')
  flag=(' †' if audit.loc[k,'Ambiguous_marker'] else '')+(' ‡' if k in shared else '')
  axa.text(1.57,yy,f'{label}{flag}  ({k})',ha='left',va='center',fontsize=namefontsize,fontstyle='italic')
 # Both scales span -1 to +1, but colorbars explicitly distinguish the units.
 c1=fig.add_axes([x+w*.01,y-.032,w*.22,.007]);cb=fig.colorbar(ScalarMappable(norm=fn,cmap='RdBu_r'),cax=c1,orientation='horizontal',ticks=[-fclimit,0,fclimit]);cb.set_label(r'$\log_2$ fold change',fontsize=6,labelpad=1);cb.ax.tick_params(labelsize=5.8,length=2,pad=1)
 c2=fig.add_axes([x+w*.36,y-.032,w*.42,.007]);cb=fig.colorbar(ScalarMappable(norm=rn,cmap='RdBu_r'),cax=c2,orientation='horizontal',ticks=[-1,-.5,0,.5,1]);cb.set_label('Correlation coefficient',fontsize=6,labelpad=1);cb.ax.tick_params(labelsize=5.8,length=2,pad=1)


display=pd.read_csv(O/'data/Heatmap_display_rows.csv')
dnra=defs[defs.Process.eq('DNRA')].KO.tolist()
grouplist=[(g,display[display.Group.eq(g)].KO.tolist()) for g in list(dict.fromkeys(display.Group))]
fig=plt.figure(figsize=(210/25.4,285/25.4))
fig.text(.025,.98,'a',fontsize=12,fontweight='bold')
gs=fig.add_gridspec(2,3,left=.08,right=.96,bottom=.655,top=.94,hspace=.62,wspace=.4)
process_panels=[('Nitrogen fixation','Nitrogen fixation'),('Ammonification','Urea hydrolysis'),('Ammonia assimilation','Ammonia assimilation'),
 ('Nitrification','Nitrification-related †'),('Denitrification','Denitrification-related †'),('DNRA','DNRA-related †')]
for i,(feature,title) in enumerate(process_panels):boxes(fig.add_subplot(gs[i//3,i%3]),feature,title)
fig.text(.025,.59,'b',fontsize=12,fontweight='bold')
matrix(fig,(.165,.102,.69,.43),display.KO.tolist(),grouplist,6.5)
save(fig,'FigS21')

assert not any(z['Outside_canvas_text'] for z in export_checks), export_checks
print('Exported the final FigS21 TIFF/SVG.')
)---",plot,useBytes=TRUE)
  if (system2(python,shQuote(plot),env=env)!=0L) stop('Plotting failed.')
  dir.create(output,recursive=TRUE,showWarnings=FALSE)
  figures <- file.path(work,c('FigS21.tiff','FigS21.svg'))
  if (!all(file.copy(figures,output,overwrite=TRUE))) stop('Could not save figures.')
  message('Exported TIFF and SVG to ',normalizePath(output))
  invisible(TRUE)
}
run_figS21()
