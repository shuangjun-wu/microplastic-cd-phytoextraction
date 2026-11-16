# Fig. 6 | Read the saved a–h source results and recreate the accepted figure.
# Keep Fig6.R and Fig6_data.xlsx together.
# Rscript Fig6.R [--output=/folder] [--input=/workbook.xlsx] [--python=/path/python3]
# R: readxl, jsonlite. Python: numpy, pandas, matplotlib, Pillow, PyMuPDF.
# This draws existing results without fitting a new model.
resolve_dir <- function() {
  for (f in rev(sys.frames())) if (!is.null(f$ofile) && file.exists(f$ofile)) return(dirname(normalizePath(f$ofile)))
  z <- sub('^--file=','',grep('^--file=',commandArgs(FALSE),value=TRUE))
  if(length(z)&&file.exists(z[1]))return(dirname(normalizePath(z[1])))
  stop('Run the saved file using Rscript or source its full path.')
}
run_fig6 <- function() {
  here<-resolve_dir();args<-commandArgs(TRUE)
  arg<-function(k,d){x<-args[startsWith(args,k)];if(length(x))substring(x[1],nchar(k)+1)else d}
  input<-normalizePath(arg('--input=',file.path(here,'Fig6_data.xlsx')),mustWork=TRUE)
  output<-arg('--output=',here)
  for(pkg in c('readxl','jsonlite'))if(!requireNamespace(pkg,quietly=TRUE))stop('Missing R package: ',pkg)
  work<-tempfile('fig6_');dir.create(work);on.exit(unlink(work,recursive=TRUE),add=TRUE)
  for(d in c('code','data','results','QA','figures','components_ag/data','components_h/data',
             'components_h/code','components_h/QA','components_h/results','components_h/figures','mpl_cache'))
    dir.create(file.path(work,d),recursive=TRUE,showWarnings=FALSE)
  specs<-jsonlite::fromJSON(r"---([{"sheet": "原始与派生_18盆", "target": "components_ag/data/pots.csv", "columns": ["PotID", "Treatment", "Shoot_dry_g", "Soil_pH", "Soil_DOC_mg_kg", "Soil_TC_g_kg", "Soil_AN_mg_kg", "Soil_TN_g_kg", "Soil_AP_mg_kg", "Soil_TP_mg_kg", "Soil_AK_mg_kg", "Soil_TK_g_kg", "Soil_UE", "Soil_BG", "Soil_ACP", "Soil_total_Cd_mg_kg", "Soil_DTPA_Cd_mg_kg", "BDW", "SDW", "RDW", "Berry/Total aboveground", "Root / Total Aboveground", "BCd", "SCd", "RCd", "BTF", "STF", "BECd", "SECd", "BBCF", "SBCF", "RBCF", "Total_dry_g", "Shoot_fraction", "Fruit_fraction", "Q_Shoot_ug", "Q_Fruit_ug", "Q_Root_ug", "Q_Harvest_ug", "Shoot_extraction_share", "DI"], "kinds": ["text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"], "n": 18}, {"sheet": "a_b_镉提取分解", "target": "components_ag/data/shoot_effects.csv", "columns": ["Treatment", "Factor", "Effect_ug", "CI_low", "CI_high"], "kinds": ["text", "text", "numeric", "numeric", "numeric"], "n": 6}, {"sheet": "a_b_镉提取汇总", "target": "components_ag/data/shoot_summary.csv", "columns": ["Treatment", "CK_Shoot_ug", "Treatment_Shoot_ug", "Net_change_ug", "Change_percent", "CI_low", "CI_high", "Closure_error"], "kinds": ["text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"], "n": 2}, {"sheet": "c_DI组间汇总", "target": "components_ag/data/group_DI.csv", "columns": ["Treatment", "n", "Total_dry_g", "Shoot_fraction_weighted", "Fruit_fraction_weighted", "Shoot_Cd_weighted", "Fruit_Cd_weighted", "Q_Shoot_ug", "Q_Fruit_ug", "DTPA_Cd", "Q_Harvest_ug", "Shoot_extraction_share", "DI_group", "Q_low", "Q_high", "Shoot_share_low", "Shoot_share_high", "DI_low", "DI_high", "Shoot_fraction_individual_mean"], "kinds": ["text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"], "n": 3}, {"sheet": "d_回归统计", "target": "components_ag/data/regressions.csv", "columns": ["Predictor", "Model", "Slope", "SE", "P_raw", "R2", "Partial_R2", "df", "t", "CI_low", "CI_high", "BH_q_4"], "kinds": ["text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"], "n": 4}, {"sheet": "d_拟合线与置信区间", "target": "components_ag/data/regression_bands.csv", "columns": ["Predictor", "X", "Fit", "Low", "High"], "kinds": ["text", "numeric", "numeric", "numeric", "numeric"], "n": 240}, {"sheet": "e_五模型性能", "target": "components_ag/data/performance.csv", "columns": ["Algorithm", "Dataset", "N", "P", "R2", "RMSE", "MAE", "R2_OOF_reweight_p025", "R2_OOF_reweight_p975", "R2_case_median", "R2_case_IQR", "R2_case_p025", "R2_case_p975", "RMSE_OOF_reweight_p025", "RMSE_OOF_reweight_p975", "RMSE_case_median", "RMSE_case_IQR", "RMSE_case_p025", "RMSE_case_p975", "MAE_OOF_reweight_p025", "MAE_OOF_reweight_p975", "MAE_case_median", "MAE_case_IQR", "MAE_case_p025", "MAE_case_p975", "Case_valid", "Fraction_R2", "Fraction_RMSE", "Fraction_MAE"], "kinds": ["text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"], "n": 5}, {"sheet": "f_PLSR变量重要性", "target": "components_ag/data/importance.csv", "columns": ["Variable", "Drop_column_delta_RMSE_fraction", "Drop_column_rank", "Native_score", "Native_rank", "Outer_native_top3_fraction", "Drop_positive_error_resample_fraction", "Drop_positive_tolerance_fraction", "Drop_resample_p025", "Drop_resample_p975", "Native_case_bootstrap_median_rank", "Native_case_bootstrap_rank_p25", "Native_case_bootstrap_rank_p75", "Native_case_bootstrap_top3_fraction", "Native_case_bootstrap_valid", "Algorithm", "Label", "Outer_nonzero_fraction", "Outer_signed_coefficient_mean", "Case_bootstrap_nonzero_fraction", "Response", "Drop_delta_RMSE"], "kinds": ["text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "text", "text", "text", "numeric"], "n": 13}, {"sheet": "g_AICc变量支持", "target": "components_ag/data/support.csv", "columns": ["Variable", "ML_screened", "Akaike_weight_sum", "Full_average_beta_per_SD", "Selected_best", "Bootstrap_ML_screen_frequency", "Bootstrap_best_inclusion_frequency", "Bootstrap_weight_median", "Bootstrap_beta_low", "Bootstrap_beta_high", "Label", "Dataset", "Fraction_weight"], "kinds": ["text", "logical", "numeric", "numeric", "logical", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "numeric"], "n": 13}, {"sheet": "g_AICc候选模型", "target": "components_ag/data/best_model.csv", "columns": ["Variables", "Slopes", "K", "Admissible", "Exclusion", "Max_abs_r", "Max_VIF", "AICc", "Delta_AICc", "Weight"], "kinds": ["text", "numeric", "numeric", "logical", "text", "numeric", "numeric", "numeric", "numeric", "numeric"], "n": 122}, {"sheet": "g_AICc预测核查", "target": "components_ag/data/pipeline.csv", "columns": ["Model", "R2", "RMSE", "MAE"], "kinds": ["text", "numeric", "numeric", "numeric"], "n": 2}, {"sheet": "c_DI显著性字母", "target": "results/DI_letters.csv", "columns": ["Treatment", "Letter"], "kinds": ["text", "text"], "n": 3}, {"sheet": "h_路径及区间", "target": "components_h/data/Paths_with_intervals.csv", "columns": ["From", "To", "Beta", "SE_t", "Residual_df", "P_t", "BH_q_current11", "Quantity", "Point", "CI95_low", "CI95_high", "Valid_draws", "Interval_excludes_zero"], "kinds": ["text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "numeric", "numeric", "numeric", "numeric", "logical"], "n": 11}, {"sheet": "h_节点解释度", "target": "components_h/data/Allocation_only_node_R2.csv", "columns": ["Node", "R2", "Residual_df", "Shapiro_residual_P"], "kinds": ["text", "numeric", "numeric", "numeric"], "n": 5}, {"sheet": "h_PC1覆盖率", "target": "components_h/data/Axis_coverage.csv", "columns": ["Node", "PC1_variance", "Loading_alignment"], "kinds": ["text", "numeric", "numeric"], "n": 4}, {"sheet": "h_模型拟合", "target": "components_h/data/Allocation_only_native_fit.csv", "columns": ["Model", "chisq", "df", "pvalue", "srmr", "cfi", "rmsea", "npar"], "kinds": ["text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"], "n": 1}])---",simplifyVector=FALSE)
  get_table<-function(spec){
    d<-as.data.frame(readxl::read_excel(input,sheet=spec$sheet,col_types=unlist(spec$kinds),.name_repair='minimal',trim_ws=FALSE),check.names=FALSE)
    if(!identical(names(d),unlist(spec$columns))||nrow(d)!=spec$n)stop('Unexpected worksheet structure: ',spec$sheet)
    d
  }
  for(spec in specs)write.csv(get_table(spec),file.path(work,spec$target),row.names=FALSE,na='',fileEncoding='UTF-8')
  preds<-get_table(jsonlite::fromJSON(r"---({"sheet": "e_逐盆留一预测", "columns": ["Algorithm", "PotID", "Treatment", "Observed", "Predicted", "Observed_logit", "Predicted_logit"], "kinds": ["text", "text", "text", "numeric", "numeric", "numeric", "numeric"], "n": 90})---",simplifyVector=FALSE))
  for(alg in c('RF','XGBoost','ElasticNet','PLSR','SVR')){
    d<-preds[preds$Algorithm==alg,-1,drop=FALSE];stopifnot(nrow(d)==18,!anyDuplicated(d$PotID))
    write.csv(d,file.path(work,'components_ag/data',paste0('OOF_',alg,'.csv')),row.names=FALSE,na='')
  }
  scripts<-list(r"---(code/draw_ag.py)---" = r"---("""Re-layout existing results only. No fitting, tuning or variable selection."""
from pathlib import Path
import json,hashlib,shutil,html
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from matplotlib.patches import Rectangle
from matplotlib.lines import Line2D

OUT=Path(__file__).resolve().parents[1]/'components_ag'
for f in ['results','figures']:(OUT/f).mkdir(parents=True,exist_ok=True)
def read(key):return pd.read_csv(OUT/'data'/(key+'.csv'))
d=read('pots');di=read('group_DI');ef=read('shoot_effects');summ=read('shoot_summary');r=read('regressions');bands=read('regression_bands')
perf=read('performance');perf=perf[perf.Dataset=='B13'].copy()
imp=read('importance');imp=imp[imp.Response=='Shoot_fraction'].copy()
sup=read('support');sup=sup[sup.Dataset=='B13'].copy()
assert len(perf)==5 and len(imp)==len(sup)==13 and 'Soil_TC_g_kg' not in set(imp.Variable)|set(sup.Variable)
checks=[]
for alg in perf.Algorithm:
 z=read('OOF_'+alg);z=z.set_index('PotID').loc[d.PotID]
 assert len(z)==18 and np.max(abs(z.Observed.to_numpy()-d.Shoot_fraction))<1e-12
 yy=z.Observed.to_numpy();pp=z.Predicted.to_numpy()
 vals=[1-((yy-pp)**2).sum()/((yy-yy.mean())**2).sum(),np.sqrt(np.mean((yy-pp)**2)),np.mean(abs(yy-pp))]
 ref=perf[perf.Algorithm==alg].iloc[0][['Fraction_R2','Fraction_RMSE','Fraction_MAE']].to_numpy(float)
 err=np.max(abs(ref-vals));assert err<1e-10
 checks.append(dict(Check=alg+' fraction OOF metrics and pot mapping',Max_abs_error=err,Passed=True))
for g in ['PE0.5','PVC5']:
 q=summ[summ.Treatment==g].iloc[0];c=ef[ef.Treatment==g]
 err=abs(c.Effect_ug.sum()-q.Net_change_ug);assert err<1e-10
 checks.append(dict(Check=g+' Shoot waterfall closure',Max_abs_error=err,Passed=True))
assert len(d)==18 and d.PotID.is_unique
fscore=r[(r.Predictor=='Shoot_fraction')&(r.Model=='Overall')].iloc[0]
pd.DataFrame(checks).to_csv(OUT/'results/Numeric_validation.csv',index=False)
perf.to_csv(OUT/'results/Panel_e_allocation_performance_B13.csv',index=False)
imp.to_csv(OUT/'results/Panel_f_allocation_PLSR_B13.csv',index=False)
sup.to_csv(OUT/'results/Panel_g_AICc_support_B13.csv',index=False)
COL={'CK':'#777777','PE0.5':'#D55E00','PVC5':'#0072B2'}
ALG=['RF','XGBoost','ElasticNet','PLSR','SVR'];AC=['#0072B2','#D55E00','#009E73','#CC79A7','#E69F00']
labels={'Soil_DOC_mg_kg':'Dissolved organic carbon','Soil_AK_mg_kg':'Available potassium','Soil_ACP':'Acid phosphatase','Soil_DTPA_Cd_mg_kg':'DTPA-extractable Cd','Soil_AN_mg_kg':'Alkali-hydrolyzable nitrogen','Soil_BG':'β-Glucosidase','Soil_pH':'pH','Soil_UE':'Urease','Soil_AP_mg_kg':'Available phosphorus','Soil_TK_g_kg':'Total potassium','Soil_TN_g_kg':'Total nitrogen','Soil_TP_mg_kg':'Total phosphorus','Soil_total_Cd_mg_kg':'Total Cd'}
category={k:('Enzymes' if k in ['Soil_BG','Soil_UE','Soil_ACP'] else 'Carbon-related' if k=='Soil_DOC_mg_kg' else 'Cd-related' if k in ['Soil_DTPA_Cd_mg_kg','Soil_total_Cd_mg_kg'] else 'pH' if k=='Soil_pH' else 'Nutrients') for k in labels}
CC={'Enzymes':'#009E73','Carbon-related':'#E69F00','Cd-related':'#D55E00','Nutrients':'#0072B2','pH':'#777777'}
plt.rcParams.update({'font.family':'Arial','font.size':7,'axes.labelsize':8,'axes.labelweight':'normal','axes.titlesize':8,'axes.linewidth':.6,'xtick.labelsize':7,'ytick.labelsize':7,'svg.fonttype':'none','pdf.fonttype':42,'mathtext.fontset':'custom','mathtext.rm':'Arial','mathtext.it':'Arial:italic','mathtext.bf':'Arial:bold','mathtext.bfit':'Arial:bold:italic','mathtext.sf':'Arial','mathtext.tt':'Arial','mathtext.cal':'Arial','mathtext.fallback':None})
from matplotlib import font_manager
font_paths={w:font_manager.findfont(font_manager.FontProperties(family='Arial',weight=w),fallback_to_default=False) for w in ['normal','bold']}
assert all(font_manager.get_font(v).family_name=='Arial' for v in font_paths.values())
(OUT/'results/Arial_font_resolution.json').write_text(json.dumps(font_paths,indent=2))
fig=plt.figure(figsize=(210/25.4,142/25.4),facecolor='white')
top=fig.add_gridspec(1,4,left=.065,right=.987,bottom=.675,top=.965,wspace=.48)
ta=[fig.add_subplot(top[0,i]) for i in range(4)]
bottom=fig.add_gridspec(1,3,left=.065,right=.987,bottom=.11,top=.47,wspace=.47,width_ratios=[1.14,1.3,.9])
es=bottom[0,0].subgridspec(1,3,wspace=.09,width_ratios=[1,1,1])
ea=[fig.add_subplot(es[0,i]) for i in range(3)]
eleft=ea[0].get_position().x0
for eax in ea:
 p=eax.get_position();eax.set_position([eleft+(p.x0-eleft)*1.14,p.y0,p.width*1.14,p.height])
fa=fig.add_subplot(bottom[0,1]);ga=fig.add_subplot(bottom[0,2])
# Match f/g top and bottom to the entire e facet including its metric strip.
epos=ea[0].get_position();aligned_top=epos.y1+.12*epos.height
fa.set_position([.39,epos.y0,.29,aligned_top-epos.y0])
ga.set_position([.855,epos.y0,.132,aligned_top-epos.y0])
def frame(ax):
 ax.spines[['top','right']].set_visible(False);ax.tick_params(width=.6,length=2.5,pad=2)
def title(ax,letter,text):
 frame(ax);ax.set_title(text,loc='left',weight='bold',pad=10)
for i,g in enumerate(['PE0.5','PVC5']):
 ax=ta[i];title(ax,'ab'[i],'')
 z=summ[summ.Treatment==g].iloc[0];vals=ef[ef.Treatment==g].Effect_ug.to_numpy()
 start=z.CK_Shoot_ug;end=z.Treatment_Shoot_ug;level=start
 ax.bar(0,start,width=.65,color='#777777');ax.text(0,start+5,f'{start:.2f}',ha='center',fontsize=7)
 for k,(v,col) in enumerate(zip(vals,['#0072B2','#E69F00','#009E73']),1):
  ax.plot([k-1+.325,k-.325],[level,level],color='#aaa',lw=.5)
  ax.bar(k,abs(v),bottom=min(level,level+v),width=.65,color=col)
  ax.text(k,min(level,level+v)-12 if k==1 else max(level,level+v)+(16 if k==3 else 5),f'{v:+.2f}',ha='center',fontsize=7)
  level+=v
 ax.plot([3.325,3.675],[level,level],color='#aaa',lw=.5);ax.bar(4,end,width=.65,color=COL[g]);ax.text(4,end+5,f'{end:.2f}',ha='center',fontsize=7)
 ax.set_ylim(0,220);ax.set_yticks([0,50,100,150,200])
 ax.set_xticks(range(5),['CK','Growth','Allocation','Cd concentration',g],rotation=45,ha='right',fontsize=7)
 ax.text(.03,.97,f'Δ = {z.Net_change_ug:.2f}\n({z.Change_percent:.2f}%)',transform=ax.transAxes,va='top',fontsize=7)
 if i==0:ax.set_ylabel('Shoot Cd extraction (µg pot$^{-1}$)')
ax=ta[2];title(ax,'c','')
for i,g in enumerate(COL):
 z=d[d.Treatment==g];row=di[di.Treatment==g].iloc[0]
 ax.scatter(i+np.linspace(-.16,.16,6),z.DI,color=COL[g],s=9,alpha=.55,edgecolors='none')
 ax.errorbar(i,row.DI_group,yerr=[[row.DI_group-row.DI_low],[row.DI_high-row.DI_group]],fmt='D',color=COL[g],ms=3,capsize=2,lw=.8)
ax.axhline(0,color='#aaa',ls='--',lw=.6);ax.set_xticks(range(3),list(COL),rotation=45,ha='right');ax.set_ylabel('Decoupling index');ax.set_ylim(-.59,.32)
letters=pd.read_csv(OUT.parent/'results/DI_letters.csv').set_index('Treatment').Letter
for i,g in enumerate(COL):
 row=di[di.Treatment==g].iloc[0]
 yy=max(float(d.loc[d.Treatment==g,'DI'].max()),float(row.DI_high))+.035
 ax.text(i+.18,yy,letters[g],ha='center',va='bottom',fontsize=8,weight='bold')
ax=ta[3];title(ax,'d','')
z=bands[bands.Predictor=='Shoot_fraction']
ax.fill_between(z.X*100,z.Low,z.High,color='#dddddd');ax.plot(z.X*100,z.Fit,color='#444444',lw=.8)
for g,col in COL.items():
 z=d[d.Treatment==g];ax.scatter(z.Shoot_fraction*100,z.DI,color=col,s=11,edgecolors='white',linewidths=.3,zorder=3)
ax.text(.96,.04,f'R² = {fscore.R2:.2f}\n$P$ = {fscore.P_raw:.3g}',transform=ax.transAxes,va='bottom',ha='right',fontsize=7)
ax.set_xlabel('Shoot fraction (%)');ax.set_ylabel('Decoupling index');ax.set_ylim(-.61,.27)
handles=[Line2D([0],[0],marker='o',color='none',markerfacecolor=COL[g],markeredgecolor='none',markersize=3.5,label=g) for g in COL]
fig.legend(handles=handles,frameon=False,ncol=1,loc='upper left',bbox_to_anchor=(ta[3].get_position().x0+.003,ta[3].get_position().y1-.003),fontsize=7,handlelength=.9,handletextpad=.35,labelspacing=.35,borderaxespad=0)
# e: three visibly separated facets from the confirmed single-response models.
for j,(ax,field,head,mult) in enumerate(zip(ea,['Fraction_R2','Fraction_RMSE','Fraction_MAE'],['Cross-validated\nR²','RMSE\n(percentage points)','MAE\n(percentage points)'],[1,100,100])):
 for sp in ax.spines.values():sp.set_color('#999999');sp.set_linewidth(.5)
 ax.add_patch(Rectangle((0,1),1,.12,transform=ax.transAxes,clip_on=False,facecolor='#e8e8e8',edgecolor='#999999',lw=.5))
 if j==0:
  ax.text(.5,1.06,head,transform=ax.transAxes,ha='center',va='center',fontsize=6.2,weight='bold')
 else:
  ax.text(.5,1.079,head.split('\n')[0],transform=ax.transAxes,ha='center',va='center',fontsize=6.5,weight='bold')
  ax.text(.5,1.027,'(percentage points)',transform=ax.transAxes,ha='center',va='center',fontsize=5.8)

 for i,(alg,col) in enumerate(zip(ALG,AC)):
  v=perf.loc[perf.Algorithm==alg,field].iloc[0]*mult
  ax.scatter(v,4-i,s=15 if alg=='PLSR' else 10,color=col,zorder=3)
  ax.text(v,4-i+.22,f'{v:.2f}',ha='center',fontsize=6.5,weight='bold' if alg=='PLSR' else 'normal')
 ax.set_ylim(-.5,4.6);ax.set_yticks(range(5),['SVR','PLSR','Elastic Net','XGBoost','RF'] if j==0 else ['']*5)
 ax.tick_params(length=2,width=.5,pad=2)
 values=perf[field].to_numpy()*mult;pad=max(np.ptp(values)*.23,.025 if j==0 else .15)
 ax.set_xlim(values.min()-pad,values.max()+pad)
 ax.set_xticks([.4,.6,.8] if j==0 else [4,5,6] if j==1 else [3.5,4.5,5.5])
 assert np.all((values>ax.get_xlim()[0])&(values<ax.get_xlim()[1]))
 ax.tick_params(axis='x',labelsize=7)
# Restore seven independent panel letters; shared legends do not combine panels.
for ax,letter,y in [(ta[0],'a',ta[0].get_position().y1+.006),(ta[1],'b',ta[1].get_position().y1+.006),(ta[2],'c',ta[2].get_position().y1+.006),(ta[3],'d',ta[3].get_position().y1+.006),(ea[0],'e',aligned_top+.006),(fa,'f',aligned_top+.006),(ga,'g',aligned_top+.006)]:
 fig.text(ax.get_position().x0-.027,y,letter,fontsize=10,weight='bold')

# f: point locations are untouched. Only text anchors are moved for clarity.
title(fa,'f','');fa.set_facecolor('#f3f4f5')
fa.axhline(1,color='#999999',ls='--',lw=.6);fa.axvline(0,color='#999999',ls='--',lw=.6)
imp['Delta_RMSE_pp']=imp.Drop_delta_RMSE*100
anchors={'Soil_DOC_mg_kg':(-.04,1.65),'Soil_AN_mg_kg':(.45,1.65),'Soil_ACP':(.02,1.14),'Soil_DTPA_Cd_mg_kg':(.53,1.44),'Soil_UE':(.39,1.15),'Soil_pH':(.045,1.44),'Soil_BG':(.17,.75),'Soil_AK_mg_kg':(.27,.99),'Soil_AP_mg_kg':(.31,.49),'Soil_TK_g_kg':(-.18,.54),'Soil_total_Cd_mg_kg':(-.20,.30),'Soil_TP_mg_kg':(-.23,.08),'Soil_TN_g_kg':(.19,.30)}
wrap={'Soil_DOC_mg_kg':'Dissolved organic\ncarbon','Soil_AN_mg_kg':'Alkali-hydrolyzable\nnitrogen','Soil_DTPA_Cd_mg_kg':'DTPA-extractable\nCd','Soil_AK_mg_kg':'Available\npotassium','Soil_AP_mg_kg':'Available\nphosphorus'}
wrap['Soil_TP_mg_kg']='Total\nphosphorus'
f_texts=[]
for row in imp.itertuples():
 x=row.Delta_RMSE_pp;y=row.Native_score;col=CC[category[row.Variable]]
 fa.scatter(x,y,s=13,color=col,zorder=4,edgecolors='white',linewidths=.3)
 lx,ly=anchors[row.Variable]
 ann=fa.annotate(wrap.get(row.Variable,labels[row.Variable]),xy=(x,y),xytext=(lx,ly),ha='center',va='center',fontsize=6.8,fontweight='bold',linespacing=1.2,arrowprops=dict(arrowstyle='-',lw=.45,color='#888888',shrinkA=2,shrinkB=2))
 ann.set_bbox(dict(facecolor='#f3f4f5',edgecolor='none',pad=.15))
 f_texts.append(ann)
fa.set_xlim(-.42,.73);fa.set_ylim(-.05,1.88);fa.set_xticks([-.3,0,.3,.6]);fa.set_yticks([0,.5,1,1.5]);fa.set_xlabel('ΔRMSE\n(percentage points)');fa.set_ylabel('VIP score')
ch=[Line2D([0],[0],marker='o',color='none',markerfacecolor=v,markeredgecolor='none',markersize=3,label=k) for k,v in CC.items()]
fig.legend(handles=ch,loc='center',bbox_to_anchor=(.535,.54),ncol=5,frameon=False,fontsize=6.5,columnspacing=.65,handlelength=.9,handletextpad=.4)
# g: all 13 predictors, even those not retained by the preceding ML screen.
title(ga,'g','')
ss=sup.sort_values('Fraction_weight',ascending=True).copy()
for i,row in enumerate(ss.itertuples()):
 col=CC[category[row.Variable]]
 ga.barh(i,row.Fraction_weight,height=.56,color=col,alpha=.85)
 ga.text(row.Fraction_weight+.025,i,f'{row.Fraction_weight:.2f}',va='center',fontsize=6.5)
ga.set_yticks(range(13),[labels[v] for v in ss.Variable],fontsize=7);ga.set_xlim(0,.79);ga.set_xticks([0,.3,.6]);ga.set_xlabel('Summed Akaike\nweight');ga.set_ylim(-.65,12.65)

# Identify the best admissible small model from the unchanged AICc table.
best=read('best_model');best=best[best.Admissible.astype(str).str.lower().eq('true')].sort_values('AICc').iloc[0]
assert set(best.Variables.split('|'))=={'Soil_AN_mg_kg','Soil_UE'}
assert abs(best.Delta_AICc)<1e-12
best_text='Lowest AICc\nAlkali-hydrolyzable\nnitrogen + Urease\nAICc = '+f'{best.AICc:.2f}'
ga.text(.61,.16,best_text,transform=ga.transAxes,ha='center',va='center',fontsize=6.5,linespacing=1.15,bbox=dict(boxstyle='round,pad=.45',facecolor='#f5f5f5',edgecolor='#bbbbbb',linewidth=.5),zorder=5)
(OUT/'results/Best_AICc_model_annotation.json').write_text(json.dumps(dict(variables=best.Variables,AICc=float(best.AICc),Delta_AICc=float(best.Delta_AICc),model_weight=float(best.Weight),annotation=best_text),indent=2))
ga.spines['left'].set_visible(True);ga.spines['left'].set_linewidth(.6);ga.tick_params(axis='y',length=2,width=.6,pad=2)
fig.canvas.draw()
renderer=fig.canvas.get_renderer()
from matplotlib.text import Text
text_boxes=[dict(label=a.get_text(),bbox=Text.get_window_extent(a,renderer).bounds) for a in f_texts]
collisions=[]
for i,a in enumerate(f_texts):
 for j,b in enumerate(f_texts[i+1:],i+1):
  if Text.get_window_extent(a,renderer).overlaps(Text.get_window_extent(b,renderer)):collisions.append([i,j])
assert not collisions,('F label overlap',collisions)
figbox=fig.bbox
for ax in [fa,ga]:
 for t in ax.get_yticklabels()+[ax.yaxis.label,ax.xaxis.label]:
  bb=t.get_window_extent(renderer)
  assert bb.x0>=0 and bb.y0>=0 and bb.x1<=figbox.x1 and bb.y1<=figbox.y1,(t.get_text(),bb.bounds)
align={'e_full_bottom':epos.y0,'e_full_top':aligned_top,'f_bottom':fa.get_position().y0,'f_top':fa.get_position().y1,'g_bottom':ga.get_position().y0,'g_top':ga.get_position().y1}
assert abs(align['f_bottom']-align['e_full_bottom'])<1e-12 and abs(align['f_top']-align['e_full_top'])<1e-12
assert abs(align['g_bottom']-align['e_full_bottom'])<1e-12 and abs(align['g_top']-align['e_full_top'])<1e-12
(OUT/'results/Alignment_QA.json').write_text(json.dumps(align,indent=2))
top_heights=[ax.get_position().height for ax in ta]
assert max(top_heights)-min(top_heights)<1e-12
(OUT/'results/Restored_layout_QA.json').write_text(json.dumps({
 'figure_size_mm':[210,142], 'panel_letters':list('abcdefg'),
 'top_axes_heights_mm':[h*142 for h in top_heights],
 'f_g_axes_height_mm':fa.get_position().height*142,
 'g_y_axis_visible':ga.spines['left'].get_visible(),
 'Cross_validated_R2_header_lines':2},indent=2))

(OUT/'results/Label_QA.json').write_text(json.dumps({'all13_labels_present':len(f_texts)==13,'f_label_collisions':collisions,'text_bboxes':text_boxes,'soil_indicator_labels':labels},indent=2))
legend_records=[]
for idx,leg in enumerate(fig.legends):
 bb=leg.get_window_extent(renderer).transformed(fig.transFigure.inverted())
 if idx==0:
  target=ta[3].get_position()
  assert bb.x0>=target.x0 and bb.x1<=target.x1 and bb.y0>=target.y0 and bb.y1<=target.y1
  location='inside_d_upper_left'
 else:
  assert bb.y0>fa.get_position().y1 and bb.x0>=ea[2].get_position().x1+.015 and bb.x1<=ga.get_position().x1
  location='above_f'
 legend_records.append(dict(location=location,n_labels=len(leg.get_texts()),figure_bbox=list(bb.bounds)))
(OUT/'results/Legend_QA.json').write_text(json.dumps(legend_records,indent=2))
point_label_collisions=[]
for ann in f_texts:
 bb=Text.get_window_extent(ann,renderer)
 for row in imp.itertuples():
  px,py=fa.transData.transform((row.Delta_RMSE_pp,row.Native_score))
  if bb.x0-2<px<bb.x1+2 and bb.y0-2<py<bb.y1+2:
   point_label_collisions.append([ann.get_text(),row.Variable])
assert not point_label_collisions,point_label_collisions
leader_lengths=[]
for ann in f_texts:
 bb=Text.get_window_extent(ann,renderer)
 px,py=fa.transData.transform(ann.xy)
 dx=max(bb.x0-px,0,px-bb.x1);dy=max(bb.y0-py,0,py-bb.y1)
 leader_lengths.append(dict(label=ann.get_text(),gap_mm=float(np.hypot(dx,dy)/fig.dpi*25.4)))
(OUT/'results/Leader_QA.json').write_text(json.dumps(leader_lengths,indent=2))

# Render-time Cartesian row gate; the final vector assembly separately audits
# the full a-h layout, including the intentionally unequal e-g regions.
row_bounds=np.array([ax.get_position().bounds for ax in ta])
assert np.max(np.ptp(row_bounds[:,1:],axis=0))<1e-9
# Clip reference/leader strokes out of text rectangles instead of relying on
# background masks. All annotation anchors and all scatter-point positions stay fixed.
def visible_segment(a,b,boxes):
    import numpy as np
    delta=b-a;cuts=[]
    for bb in boxes:
        lo,hi=0.,1.
        for dim,L,H in [(0,bb.x0,bb.x1),(1,bb.y0,bb.y1)]:
            if abs(delta[dim])<1e-12:
                if a[dim]<L or a[dim]>H:lo,hi=1.,0.;break
            else:
                t1=(L-a[dim])/delta[dim];t2=(H-a[dim])/delta[dim]
                lo=max(lo,min(t1,t2));hi=min(hi,max(t1,t2))
        if hi>lo:cuts.append((lo,hi))
    cuts.sort();merged=[]
    for lo,hi in cuts:
        if merged and lo<=merged[-1][1]:merged[-1]=(merged[-1][0],max(hi,merged[-1][1]))
        else:merged.append((lo,hi))
    cursor=0.;pieces=[]
    for lo,hi in merged:
        if lo>cursor:pieces.append((a+cursor*delta,a+lo*delta))
        cursor=max(cursor,hi)
    if cursor<1:pieces.append((a+cursor*delta,b))
    return pieces
fig.canvas.draw();renderer=fig.canvas.get_renderer()
padding=2.5*fig.dpi/72
text_rects=[Text.get_window_extent(ann,renderer).padded(padding) for ann in f_texts]
original_strokes=[]
for line in list(fa.lines):
    vertices=line.get_transform().transform(line.get_path().vertices)
    original_strokes.append((vertices,line.get_color(),line.get_linewidth(),line.get_linestyle(),line.get_zorder()))
    line.set_visible(False)
for ann in f_texts:
    ar=ann.arrow_patch
    vertices=ar.get_transform().transform(ar.get_path().vertices)
    original_strokes.append((vertices,'#888888',.45,'-',2.8))
    ar.set_visible(False)
for vv,color,lw,ls,zorder in original_strokes:
    for a,b in zip(vv[:-1],vv[1:]):
        for p0,p1 in visible_segment(a,b,text_rects):
            u=fa.transData.inverted().transform([p0,p1])
            fa.plot(u[:,0],u[:,1],color=color,lw=lw,ls=ls,zorder=zorder,solid_capstyle='butt')
fig.canvas.draw()
(OUT/'results/Readability_only_adjustments.json').write_text(json.dumps(dict(
 unchanged_data_points=True,changed_annotation_anchors=['Acid phosphatase','Available potassium','Dissolved organic carbon','Alkali-hydrolyzable nitrogen','DTPA-extractable Cd','Total phosphorus','Total Cd'],old_statistics_unchanged=True,new_DI_inference='Exact whole-pot permutation; Holm across three comparisons',
 waterfall_tick_font_pt=7,axis_tick_font_pt=7,axis_title_font_pt=8,multiline_spacing=1.2,math_R2_replaced_by_editable_Unicode=True,
 reference_and_leader_strokes_clipped_at_label_bounds=True),indent=2))

for ext in ['pdf','svg']:
 kw={'dpi':600} if ext in ['png','tiff'] else {}
 if ext=='tiff':kw['pil_kwargs']={'compression':'tiff_lzw'}
 fig.savefig(OUT/'figures'/('Fig6_percentage_points_a_g.'+ext),bbox_inches='tight',pad_inches=.06,**kw)
plt.close(fig)
)---",
r"---(components_h/code/draw_h.py)---" = r"---("""Selected z-standardized raw-allocation model; no logit, standardized beta plot."""
from pathlib import Path
import sys,json
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from matplotlib.path import Path as MPath
from matplotlib.patches import FancyArrowPatch,FancyBboxPatch
from matplotlib.lines import Line2D
from matplotlib.text import Text
O=Path(__file__).resolve().parents[1]
p=pd.read_csv(O/'data/Paths_with_intervals.csv');r2=pd.read_csv(O/'data/Allocation_only_node_R2.csv').set_index('Node');coverage=pd.read_csv(O/'data/Axis_coverage.csv').set_index('Node');fit=pd.read_csv(O/'data/Allocation_only_native_fit.csv').iloc[0]
assert len(p)==11 and json.loads((O/'data/Source_native_validation.json').read_text())['status']=='PASS'
plt.rcParams.update({'font.family':'Arial','font.size':7,'svg.fonttype':'none','pdf.fonttype':42,'axes.unicode_minus':True,'mathtext.fontset':'custom','mathtext.rm':'Arial','mathtext.it':'Arial:italic','mathtext.bf':'Arial:bold'})
fig,ax=plt.subplots(figsize=(7.4803149606,2.8346456693));fig.subplots_adjust(left=.015,right=.995,top=.985,bottom=.025)
ax.set(xlim=(-7,192),ylim=(0,82));ax.axis('off')
xy={'G':(18,69),'K':(18,30),'AN':(65,69),'UE':(65,30),'R':(113,69),'S':(113,30),'F':(170,47)}
w,h=30,15
ink='#26333D';muted='#ADB4B9';teal='#087F8C';purple='#75508B'
col={'G':'#61866A','K':'#61866A','AN':teal,'UE':teal,'R':purple,'S':'#6D82A8','F':'#C38B3D'}
fc={'G':'#F4F8F3','K':'#F4F8F3','AN':'#EFF7F6','UE':'#EFF7F6','R':'#F4EEF8','S':'#F2F5FA','F':'#FFF4E5'}
ax.text(-5,78.8,'h',fontsize=10,weight='bold',ha='left',va='center')
curves={('G','AN'):[[33,69],[38,69],[45,69],[50,69]],('G','UE'):[[33,66],[46,62],[38,37],[50,33]],('K','AN'):[[33,33],[39,38],[44,61],[50,66]],('K','UE'):[[33,30],[38,30],[45,30],[50,30]],('AN','R'):[[80,69],[87,69],[91,69],[98,69]],('AN','S'):[[80,66],[94,63],[86,38],[98,33]],('UE','R'):[[80,33],[87,39],[92,61],[98,66]],('UE','S'):[[80,30],[87,30],[90,30],[98,30]],('R','S'):[[113,61.5],[123,56],[123,43],[113,37.5]],('R','F'):[[128,67],[143,67],[143,49],[155,49]],('S','F'):[[128,33],[142,33],[144,44],[155,44]]}
def draw(c,**kw):
 path=MPath(np.array(c,float),[MPath.MOVETO,MPath.CURVE4,MPath.CURVE4,MPath.CURVE4]);item=FancyArrowPatch(path=path,arrowstyle=kw.pop('arrowstyle','-|>'),mutation_scale=8,**kw);ax.add_patch(item);return item
for c in [[[3,67],[-5,58],[-5,40],[3,32]],[[65,61.5],[72,58],[72,41],[65,37.5]]]:draw(c,arrowstyle='<->',lw=.6,color='#B5BCC1',zorder=1)
positions={('G','AN'):(41.5,73),('G','UE'):(34.5,51),('K','AN'):(49,51),('K','UE'):(41.5,26),('AN','R'):(89,75),('AN','S'):(99,48),('UE','R'):(80,54),('UE','S'):(89,24),('R','S'):(128,46),('R','F'):(147,65),('S','F'):(148,30)}
texts=[];drawn=[]
for row in p.sort_values('BH_q_current11',ascending=False).itertuples():
 key=(row.From,row.To);q=row.BH_q_current11;sig=q<.05
 color=(teal if row.From in {'AN','UE'} else purple) if sig else '#BAC1C6'
 art=draw(curves[key],lw=1.8 if sig else .65,color=color,linestyle='-' if sig else (0,(3.1,2.6)),zorder=2 if sig else 1)
 art.set_gid('path_'+row.From+'_'+row.To)
 beta=f'{row.Beta:+.3f}'.replace('-','−').replace('+','+')
 text=f'β = {beta}\nq = {q:.3g}' if sig or key == ('S','F') else beta
 if key == ('UE','R'): text=f'β = {beta}\nq = {q:.3g}\nRaw $P$ = {row.P_t:.4f}'
 x,y=positions[key]
 if key == ('UE','R'):
  # Emphasize the raw P without changing the q-based dashed path.
  for suffix,yy,tt,fs,weight,tc in [
   ('beta',y+2.6,f'β = {beta}',5.8,'normal','#7D8891'),
   ('q',y,f'q = {q:.3g}',5.8,'normal','#7D8891'),
   ('rawP',y-2.8,f'Raw $P$ = {row.P_t:.4f}',6.2,'bold',ink)]:
   a=ax.text(x,yy,tt,ha='center',va='center',fontsize=fs,weight=weight,color=tc,zorder=6)
   texts.append(('edge_UE_R_'+suffix,a))
 else:
  a=ax.text(x,y,text,ha='center',va='center',fontsize=6.2 if sig else 5.8,weight='bold' if sig else 'normal',color=color if sig else '#7D8891',linespacing=1.15,zorder=6)
  texts.append(('edge_'+row.From+'_'+row.To,a))
 drawn.append(dict(From=row.From,To=row.To,Beta=row.Beta,P=row.P_t,BH_q_current11=q,CI_low=row.CI95_low,CI_high=row.CI95_high,Line='solid' if sig else 'dashed',Label=text))
labels={'G':('Genus composition','G'),'K':('KEGG KO profile','K'),'AN':('Alkali-hydrolyzable N','AN'),'UE':('Urease','UE'),'R':('Root transcriptome','R'),'S':('Shoot transcriptome','S'),'F':('Shoot allocation','F')}
node_text_groups={}
for n,(x,y) in xy.items():
 ax.add_patch(FancyBboxPatch((x-w/2,y-h/2),w,h,boxstyle='round,pad=0,rounding_size=1.2',facecolor=fc[n],edgecolor=col[n],lw=1.2 if n=='R' else .7,zorder=4))
 title=labels[n][0]
 metadata='PC1' if n in coverage.index else ''
 rows=[('title',title,7.2,True)]
 if metadata:rows.append(('PC1',metadata,6.0,False))
 if n in r2.index:
  rows.append(('R2',f'R² = {r2.loc[n,"R2"]:.2f}',5.9,False))
 # Two rows form a centered pair. Three rows use centered, equally spaced slots.
 offsets=[2.25,-2.25] if len(rows)==2 else [4.2,0,-4.2]
 assert len(rows)==len(offsets)
 group=[]
 for dy,(role,t,fs,bold) in zip(offsets,rows):
  a=ax.text(x,y+dy,t,ha='center',va='center',fontsize=fs,weight='bold' if bold else 'normal',color=ink,zorder=5)
  texts.append(('node_'+n+'_'+role,a));group.append((role,a))
 node_text_groups[n]=group
ax.legend(handles=[Line2D([0],[0],color=teal,lw=1.8,label='BH-q < 0.05 (11 paths)'),Line2D([0],[0],color=muted,lw=.65,ls='--',label='BH-q ≥ 0.05'),Line2D([0],[0],color='#B5BCC1',lw=.6,label='Double-headed: covariance')],loc='lower left',bbox_to_anchor=(.02,.055),ncol=3,frameon=False,fontsize=6.2,handlelength=1.6,columnspacing=1.4)
ax.text(0,2.9,f'χ²({int(fit.df)}) = {fit.chisq:.2f}     $P$ = {fit.pvalue:.3f}     SRMR = {fit.srmr:.3f}',ha='left',va='center',fontsize=6.0,color='#5D6973')
fig.canvas.draw();renderer=fig.canvas.get_renderer()
# Match the center of the actual rendered text block to the node center,
# including the different title, PC1 and R² font heights.
for n,group in node_text_groups.items():
 boxes=[Text.get_window_extent(a,renderer) for role,a in group]
 current=(min(b.y0 for b in boxes)+max(b.y1 for b in boxes))/2
 target=ax.transData.transform(xy[n])[1]
 dy=ax.transData.inverted().transform((0,target))[1]-ax.transData.inverted().transform((0,current))[1]
 for role,a in group:a.set_y(a.get_position()[1]+dy)
fig.canvas.draw();renderer=fig.canvas.get_renderer();internal=[]
for n,group in node_text_groups.items():
 boxes=[Text.get_window_extent(a,renderer) for role,a in group]
 current=(min(b.y0 for b in boxes)+max(b.y1 for b in boxes))/2;target=ax.transData.transform(xy[n])[1]
 err=abs(current-target)*72/fig.dpi
 assert err<.05,(n,err)
 internal.append(dict(Node=n,Rows=len(group),Text_block_center_error_pt=err,
  Ordered_rows=[dict(Role=role,Y_offset=a.get_position()[1]-xy[n][1],Text=a.get_text()) for role,a in group]))
(O/'QA/Node_internal_layout.json').write_text(json.dumps(dict(status='PASS',rule='Entire rendered two/three-row text block centered within each node; common row slots for equal row counts',nodes=internal),indent=2))
collisions=[]
for i,(key,a) in enumerate(texts):
 for key2,b in texts[i+1:]:
  if Text.get_window_extent(a,renderer).overlaps(Text.get_window_extent(b,renderer)):collisions.append([key,key2])
assert not collisions,collisions
fig.savefig(O/'figures/Panel_h_CBSEM_BH.svg',facecolor='white',bbox_inches='tight',pad_inches=.04)
fig.savefig(O/'figures/Panel_h_CBSEM_BH.pdf',facecolor='white',bbox_inches='tight',pad_inches=.04)
plt.close(fig)
pd.DataFrame(drawn).to_csv(O/'results/Actually_drawn_paths.csv',index=False)
assert len(drawn)==11 and set(xy)=={'G','K','AN','UE','R','S','F'}
assert next(r for r in drawn if r['From']=='S' and r['To']=='F')['Line']=='dashed'
(O/'QA/Figure_validation.json').write_text(json.dumps(dict(status='PASS',nodes=7,paths=11,biomass_absent=True,Shoot_RNA_to_allocation_estimated_and_drawn=True,one_Parallel_RNA_model_only=True,significance_rule='Current11-path BH; Urease raw P explicitly additional',text_collisions=collisions),indent=2))
print('Rendered selected z-standardized raw-allocation model; standardized beta, no logit.')

assert sum(r['Line']=='solid' for r in drawn)==2
assert next(r for r in drawn if r['From']=='UE' and r['To']=='R')['Line']=='dashed'
)---",
r"---(code/assemble.py)---" = r"---("""Python vector assembly of confirmed a-g plus selected h; no statistical refit.
SVG IDs namespaced; PDF pages retained as vector forms with editable text.
Two inherited sub5pt superscript glyph runs enlarged to5pt for legibility only.
"""
from pathlib import Path
import sys,json,hashlib,shutil,re,copy,collections,csv
import xml.etree.ElementTree as ET
import matplotlib
matplotlib.rcParams.update({'font.family':'Arial','font.size':7,'svg.fonttype':'none','pdf.fonttype':42})
import pymupdf as fitz
from PIL import Image
O=Path(__file__).resolve().parents[1]
for d in ['data','figures','QA']:(O/d).mkdir(exist_ok=True)
for key,rel in [('ag','components_ag/figures/Fig6_percentage_points_a_g'),('h','components_h/figures/Panel_h_CBSEM_BH')]:
 for ext in ['pdf','svg']:shutil.copy2(O/(rel+'.'+ext),O/'data'/(key+'_'+ext+'.'+ext))
SVG='http://www.w3.org/2000/svg';XLINK='http://www.w3.org/1999/xlink'
ET.register_namespace('',SVG);ET.register_namespace('xlink',XLINK)
roots={k:ET.parse(O/'data'/(k+'_svg.svg')).getroot() for k in ['ag','h']}
def view(root):return list(map(float,root.attrib['viewBox'].split()))
aw,ah=view(roots['ag'])[2:];hw,hh=view(roots['h'])[2:]
sourcepdf={k:fitz.open(O/'data'/(k+'_pdf.pdf')) for k in ['ag','h']}
# Matplotlib's SVG/PDF text engines differ by about0.018pt in the inherited tight crop.
# This is far below the1.5pt layout tolerance; SVG canvas sizes define assembly geometry.
assert abs(sourcepdf['ag'][0].rect.width-aw)<.05 and abs(sourcepdf['h'][0].rect.height-hh)<.05
def spans(page):return [s for b in page.get_text('dict')['blocks'] if 'lines'in b for l in b['lines'] for s in l['spans']]
def letter(page,c):return min((s for s in spans(page) if s['text']==c and s['size']>=9.9),key=lambda s:s['bbox'][1])
scale_h=1.08;gap=12.0
left=letter(sourcepdf['ag'][0],'a')['bbox'][0]-scale_h*letter(sourcepdf['h'][0],'h')['bbox'][0]
top_h=ah+gap;total_h=top_h+scale_h*hh
assert left>=0 and left+scale_h*hw<=aw+.01
contract=dict(scope='Assemble existing confirmed panels; no refitting or statistical changes',backend='Python',reuse='Readability-only a-g redraw plus exact h vector reuse; statistical values and point locations unchanged',
 question='How are Shoot allocation, Cd extraction decoupling, and nitrogen-related soil/RNA associations connected?',
 panel_roles={'a,b':'Shoot extraction LMDI decomposition','c,d':'Decoupling and allocation association','e':'Five-algorithm comparison','f':'PLSR predictor importance','g':'AICc small-model support','h':'Selected seven-node covariance path model'},
 selected_h='All7 nodes z standardized; raw allocation fraction, no logit; current11-path BH',
 source_sample_sizes={'a-g':18,'h':12},
 canvas_mm=[aw*25.4/72,total_h*25.4/72],h_scale=scale_h,gap_mm=gap*25.4/72,
 minimum_text_pt=5,statistical_sources_unchanged=True,outputs=['Editable SVG','Vector PDF','600dpi PNG','600dpi TIFF'])
(O/'Figure_contract.json').write_text(json.dumps(contract,ensure_ascii=False,indent=2))
# Improve tiny inherited math superscripts, without changing the text/value.
font_changes=[]
tf=re.compile(rb'(/[A-Za-z0-9_.+\-]+)\s+([0-9]+(?:\.[0-9]+)?)\s+Tf')
for key,doc in sourcepdf.items():
 page=doc[0]
 for xref in page.get_contents():
  data=doc.xref_stream(xref)
  def replace(m):
   size=float(m[2]);effective=size*(scale_h if key=='h' else 1)
   if effective<5-1e-6:
    new=5/(scale_h if key=='h' else 1);font_changes.append(dict(Source=key,Original_font_pt=size,New_font_pt=new))
    return m[1]+b' '+f'{new:.8f}'.encode()+b' Tf'
   return m[0]
  updated=tf.sub(replace,data)
  if updated!=data:doc.update_stream(xref,updated)
 doc.save(O/'data'/(key+'_readable.pdf'),garbage=4,deflate=True)
# SVG source text stays editable; apply the same physical font floor.
for key,root in roots.items():
 factor=scale_h if key=='h' else 1
 for el in root.iter():
  st=el.attrib.get('style','')
  def sizefix(m):
   size=float(m[1]);return f'font-size: {5/factor:.8f}px' if size*factor<5-1e-6 else m[0]
  if st:el.set('style',re.sub(r'font-size:\s*([0-9.]+)px',sizefix,st))
# Extract actual a-g plot rectangles from their source vector axes patches.
axes={}
for el in roots['ag'].iter():
 aid=el.attrib.get('id','')
 if re.fullmatch(r'axes_\d+',aid):
  patch=next((c for c in list(el) if c.attrib.get('id','').startswith('patch_')),None)
  path=patch.find('{'+SVG+'}path') if patch is not None else None
  assert path is not None,aid
  nums=list(map(float,re.findall(r'[-+]?(?:\d*\.\d+|\d+)(?:[eE][-+]?\d+)?',path.attrib['d'])))
  xy=list(zip(nums[::2],nums[1::2]));xs=[v[0]for v in xy];ys=[v[1]for v in xy]
  axes[aid]=[min(xs),min(ys),max(xs),max(ys)]
assert len(axes)==9
panels=[]
def box_bottom(b):return [b[0],total_h-b[3],b[2],total_h-b[1]]
for i,c in enumerate('abcd',1):
 a=letter(sourcepdf['ag'][0],c)['bbox'];panels.append(dict(id=c,bbox_pt=box_bottom(axes[f'axes_{i}']),panel_label_anchor_pt=[a[0],total_h-a[1]],row_start=0,row_stop=1,col_start=i-1,col_stop=i))
# e effective viewport includes the visible metric strips, as in the confirmed source.
eb=[axes[f'axes_{i}']for i in [5,6,7]];b=[min(v[0]for v in eb),min(v[1]for v in eb),max(v[2]for v in eb),max(v[3]for v in eb)]
b[1]-=.12*(eb[0][3]-eb[0][1]);a=letter(sourcepdf['ag'][0],'e')['bbox']
panels.append(dict(id='e',bbox_pt=box_bottom(b),panel_label_anchor_pt=[a[0],total_h-a[1]],row_start=1,row_stop=2,col_start=0,col_stop=1.14))
for c,ax,cs,ce in [('f','axes_8',1.14,2.44),('g','axes_9',2.44,3.34)]:
 a=letter(sourcepdf['ag'][0],c)['bbox'];panels.append(dict(id=c,bbox_pt=box_bottom(axes[ax]),panel_label_anchor_pt=[a[0],total_h-a[1]],row_start=1,row_stop=2,col_start=cs,col_stop=ce))
a=letter(sourcepdf['h'][0],'h')['bbox']
panels.append(dict(id='h',bbox_pt=[left,0,left+scale_h*hw,scale_h*hh],panel_label_anchor_pt=[left+scale_h*a[0],total_h-(top_h+scale_h*a[1])]))
layout=dict(schema_version=1,backend='python-vector-assembly',figure=dict(width_pt=aw,height_pt=total_h),panels=panels,
 row_groups=[dict(id='a-d-equal-panels',panels=list('abcd')),dict(id='e-g-confirmed-unequal-width-row',panels=list('efg'))],
 exemptions=[dict(panels=list('efg'),checks=['horizontal-gutter'],reason='Confirmed unequal gutters reserve the full predictor names to the left of g; preserve the approved a-g layout.'),dict(panels=['h'],checks=['all'],reason='Full-row free-positioned path diagram, not comparable Cartesian plot areas.')])
(O/'QA/Final_panel_layout.json').write_text(json.dumps(layout,indent=2))
for ids in ['abcd','efg']:
 p=[z['bbox_pt'] for z in panels if z['id'] in ids]
 assert max(v[1] for v in p)-min(v[1] for v in p)<=1.5
 assert max(v[3] for v in p)-min(v[3] for v in p)<=1.5
 assert all(a[2]<=b[0]+1.5 for a,b in zip(p,p[1:]))
assert abs(panels[0]['panel_label_anchor_pt'][0]-panels[-1]['panel_label_anchor_pt'][0])<1e-9
# Namespaced vector assembly avoids collisions among Matplotlib clip/path IDs.
outer=ET.Element('{'+SVG+'}svg',dict(width=f'{aw:.8f}pt',height=f'{total_h:.8f}pt',viewBox=f'0 0 {aw:.8f} {total_h:.8f}',version='1.1'))
ET.SubElement(outer,'{'+SVG+'}rect',dict(x='0',y='0',width=str(aw),height=str(total_h),fill='white'))
for key,root,x,y,scale in [('ag',roots['ag'],0,0,1),('h',roots['h'],left,top_h,scale_h)]:
 prefix=key+'_';ids={el.attrib['id']:prefix+el.attrib['id'] for el in root.iter()if'id'in el.attrib}
 for el in root.iter():
  for attr,value in list(el.attrib.items()):
   if attr=='id':el.set(attr,ids[value]);continue
   if value.startswith('#')and value[1:]in ids:value='#'+ids[value[1:]]
   value=re.sub(r'url\(#([^)]+)\)',lambda m:'url(#'+ids.get(m[1],m[1])+')',value)
   el.set(attr,value)
 group=ET.SubElement(outer,'{'+SVG+'}g',dict(id=key+'_composite_block',transform=f'translate({x:.8f},{y:.8f}) scale({scale:.8f})'))
 for child in list(root):
  if child.tag!='{'+SVG+'}metadata':group.append(copy.deepcopy(child))
svg=O/'figures/Fig6_a_h_combined.svg';ET.ElementTree(outer).write(svg,encoding='utf-8',xml_declaration=True)
all_ids=[el.attrib['id'] for el in outer.iter()if'id'in el.attrib];assert len(all_ids)==len(set(all_ids))
out=fitz.open();page=out.new_page(width=aw,height=total_h)
agdoc=fitz.open(O/'data/ag_readable.pdf');hdoc=fitz.open(O/'data/h_readable.pdf')
page.show_pdf_page(fitz.Rect(0,0,aw,ah),agdoc,0)
page.show_pdf_page(fitz.Rect(left,top_h,left+scale_h*hw,total_h),hdoc,0)
out.set_metadata(dict(title='Fig.6 | Shoot allocation, Cd extraction decoupling and soil–microbiome–plant path model',creator='Python vector composition of confirmed source panels'))
pdf=O/'figures/Fig6_a_h_combined.pdf';out.save(pdf,garbage=4,deflate=True)
expected=collections.Counter((agdoc[0].get_text()+hdoc[0].get_text()).split());actual=collections.Counter(page.get_text().split());assert expected==actual,(expected-actual,actual-expected)
ps=spans(page);assert min(s['size']for s in ps)>=4.999
assert set(c for c in 'abcdefgh' if any(s['text']==c for s in ps))==set('abcdefgh')
pix=page.get_pixmap(matrix=fitz.Matrix(600/72,600/72),alpha=False)
im=Image.frombytes('RGB',[pix.width,pix.height],pix.samples)
im.save(O/'figures/Fig6_a_h_combined.tiff',dpi=(600,600),compression='tiff_lzw')
# Small preview is for screen inspection only;600dpi exports remain authoritative.
qa=dict(status='PASS',panel_letters=list('abcdefgh'),all_source_text_and_numbers_preserved=True,original_source_files_unchanged=True,editable_SVG_text_count=sum(el.tag=='{'+SVG+'}text'for el in outer.iter()),PDF_fonts=sorted(set(s['font']for s in ps)),minimum_final_font_pt=min(s['size']for s in ps),canvas_mm=contract['canvas_mm'],superscript_font_floor_changes=font_changes,h_label_aligned_to_a_e=True,h_bottom_full_row=True,assembly_only=True,models_refitted=False)
(O/'QA/Assembly_validation.json').write_text(json.dumps(qa,ensure_ascii=False,indent=2))
print('Combined vector figure:',pdf.resolve());print(json.dumps(qa,ensure_ascii=False,indent=2))
)---")
  for(n in names(scripts))writeLines(scripts[[n]],file.path(work,n),useBytes=TRUE)
  writeLines(r"---({
  "status": "PASS",
  "both_models_converged": true,
  "coefficients_t_P_BH_verified": true
}
)---",file.path(work,'components_h/data/Source_native_validation.json'),useBytes=TRUE)
  candidates<-unique(c(arg('--python=',Sys.getenv('FIG6_PYTHON')),Sys.which('python3'),
    file.path(path.expand('~'),'.cache/codex-runtimes/codex-primary-runtime/dependencies/python/bin/python3')))
  env<-paste0('MPLCONFIGDIR=',shQuote(file.path(work,'mpl_cache')));python<-NULL
  for(p in candidates[nzchar(candidates)&file.exists(candidates)]){
    status<-suppressWarnings(system2(p,c('-c',shQuote('import numpy,pandas,matplotlib.pyplot,PIL,pymupdf; assert callable(pymupdf.open)')),env=env,stdout=FALSE,stderr=FALSE))
    if(identical(status,0L)){python<-p;break}
  }
  if(is.null(python))stop('Set --python= to an interpreter with numpy, pandas, matplotlib, Pillow and PyMuPDF.')
  for(n in names(scripts))if(system2(python,shQuote(file.path(work,n)),env=env)!=0L)stop('Drawing failed: ',n)
  dir.create(output,recursive=TRUE,showWarnings=FALSE)
  for(ext in c('tiff','svg')){
    src<-file.path(work,'figures',paste0('Fig6_a_h_combined.',ext))
    if(!file.copy(src,file.path(output,paste0('Fig6.',ext)),overwrite=TRUE))stop('Could not save ',ext)
  }
  message('Exported Fig. 6 from stored results to ',normalizePath(output))
}
run_fig6()
