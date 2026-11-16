# Fig. S11: reproduce the accepted compact figure from its companion workbook.
# Run: Rscript FigS11.R [--input=/data.xlsx] [--output=/folder]
# The original statistics and display selection are preserved.
resolve_dir <- function() {
  for (f in rev(sys.frames())) if(!is.null(f$ofile)&&file.exists(f$ofile))return(dirname(normalizePath(f$ofile)))
  z<-sub('^--file=','',grep('^--file=',commandArgs(),value=TRUE))
  if(length(z)&&file.exists(z[1]))return(dirname(normalizePath(z[1])))
  stop('Run the saved script with Rscript or source its full path.')
}
here<-resolve_dir();args<-commandArgs(TRUE)
arg<-function(k,d){x<-args[startsWith(args,k)];if(length(x))substring(x[1],nchar(k)+1)else d}
input<-normalizePath(arg('--input=',file.path(here,'FigS11_data.xlsx')),mustWork=TRUE)
dest<-arg('--output=',here);figure_number<-11L
# Layout keys retain the original numbered template; the exported name uses the current figure number.
n<-12L
pkgs<-c('readxl','data.table','ggplot2','jsonlite','ragg','svglite')
missing<-pkgs[!vapply(pkgs,requireNamespace,logical(1),quietly=TRUE)]
if(length(missing))stop('Missing R packages: ',paste(missing,collapse=', '))
base<-tempfile('figS11_');dir.create(base)
suppressPackageStartupMessages({library(data.table);library(ggplot2);library(grid);library(jsonlite);library(ragg);library(svglite)})
samples<-c(paste0('CKS_',1:4),paste0('PES_',1:4),paste0('PVCS_',1:4))
groups<-c('CK','PE0.5','PVC5');group<-factor(rep(groups,each=4),levels=groups)
cols<-c(CK='#3B5387',PE0.5='#009F86',PVC5='#E54B34')
theme_set(theme_classic(base_size=7.5,base_family='Arial')+theme(
 axis.title=element_text(size=8.5,colour='black'),axis.text=element_text(size=7.5,colour='black'),
 legend.title=element_text(size=8.5),legend.text=element_text(size=7.5),
 plot.title=element_text(size=8.5,face='plain',hjust=0),
 strip.text=element_text(size=7.5,face='plain'),strip.background=element_blank(),
 axis.line=element_line(linewidth=.35),axis.ticks=element_line(linewidth=.35),
 legend.key.height=unit(3.8,'mm'),legend.key.width=unit(3.2,'mm'),legend.spacing.y=unit(.3,'mm'),
 plot.margin=margin(3,3,2,3,'mm'),panel.spacing=unit(3,'mm')))
pfmt<-function(p)ifelse(p<.001,'< 0.001',paste0('= ',formatC(p,digits=3,format='f')))
pcoa<-function(com){z<-com$result;d<-com$coords;ell<-rbindlist(lapply(groups,function(g){v<-as.matrix(d[Treatment==g,.(Axis1,Axis2)]);th<-seq(0,2*pi,len=121);xy<-sweep(sqrt(qchisq(.95,2))*cbind(cos(th),sin(th))%*%chol(cov(v)),2,colMeans(v),'+');data.table(Treatment=factor(g,levels=groups),Axis1=xy[,1],Axis2=xy[,2])}))
 ggplot(d,aes(Axis1,Axis2,colour=Treatment,shape=Treatment))+geom_path(data=ell,aes(group=Treatment),inherit.aes=TRUE,linewidth=.5,show.legend=FALSE)+geom_point(size=2)+
 scale_colour_manual(values=cols)+scale_shape_manual(values=c(16,17,15))+labs(x=sprintf('PCoA 1 (%.1f%%)',z$PCoA1_percent),y=sprintf('PCoA 2 (%.1f%%)',z$PCoA2_percent),colour=NULL,shape=NULL)+
 annotate('text',x=-Inf,y=Inf,hjust=-.03,vjust=1.1,family='Arial',size=7/.pt,label=sprintf('PERMANOVA: R² = %.3f, P %s\nPERMDISP: P %s',z$R2,pfmt(z$P_PERMANOVA),pfmt(z$P_PERMDISP)))+
 scale_y_continuous(expand=expansion(mult=c(.08,.37)))+theme(panel.border=element_rect(fill=NA,colour='black',linewidth=.35),axis.line=element_blank(),legend.position='inside',legend.position.inside=c(.97,.97),legend.justification=c(1,1),legend.background=element_blank())
}

specs<-jsonlite::fromJSON(r"---([{"name": "b_PCoA坐标", "key": "CAZy_Class_RPKM_PCoA", "columns": ["Sample", "Treatment", "Axis1", "Axis2"], "kinds": ["text", "text", "numeric", "numeric"], "n": 12}, {"name": "b_群落检验", "key": "CAZy_Class_RPKM_community", "columns": ["Profile", "Features", "R2", "F", "P_PERMANOVA", "P_PERMDISP", "PCoA1_percent", "PCoA2_percent", "Negative_inertia_percent", "Allocations"], "kinds": ["text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"], "n": 1}, {"name": "e_PCoA坐标", "key": "CAZy_Family_RPKM_PCoA", "columns": ["Sample", "Treatment", "Axis1", "Axis2"], "kinds": ["text", "text", "numeric", "numeric"], "n": 12}, {"name": "e_群落检验", "key": "CAZy_Family_RPKM_community", "columns": ["Profile", "Features", "R2", "F", "P_PERMANOVA", "P_PERMDISP", "PCoA1_percent", "PCoA2_percent", "Negative_inertia_percent", "Allocations"], "kinds": ["text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"], "n": 1}, {"name": "c_组成图", "key": "Native_Class_composition", "columns": ["Feature", "Label", "Treatment", "Mean_relative", "Color"], "kinds": ["text", "text", "text", "numeric", "text"], "n": 21}, {"name": "f_组成图", "key": "Native_Family_composition", "columns": ["Feature", "Label", "Treatment", "Mean_relative", "Color"], "kinds": ["text", "text", "text", "numeric", "text"], "n": 153}, {"name": "g_LEfSe", "key": "Native_LEfSe_g", "columns": ["Feature", "Treatment", "LDA", "P_original", "Value_source", "Source_PDF", "Original_PDF_LDA", "Row"], "kinds": ["text", "text", "numeric", "numeric", "text", "text", "numeric", "numeric"], "n": 15}, {"name": "a_共有与特有", "key": "Native_Venn_a", "columns": ["Membership_code", "Features"], "kinds": ["numeric", "numeric"], "n": 7}, {"name": "d_共有与特有", "key": "Native_Venn_d", "columns": ["Membership_code", "Features"], "kinds": ["numeric", "numeric"], "n": 7}, {"name": "面板对应表", "key": "Panel_order_and_sources", "columns": ["Panel", "Level", "Content", "Current_source", "Rendering"], "kinds": ["text", "text", "text", "text", "text"], "n": 7}])---",simplifyVector=FALSE)
tables<-list()
for(item in specs){
  d<-readxl::read_excel(input,sheet=item$name,col_types=unlist(item$kinds),.name_repair='minimal',trim_ws=FALSE)
  stopifnot(identical(names(d),unlist(item$columns)),nrow(d)==item$n)
  tables[[item$key]]<-as.data.table(d)
}
res<-function(name)copy(tables[[name]])
spec <- res('Panel_order_and_sources')
stopifnot(identical(spec$Panel,letters[seq_len(nrow(spec))]))
W <- 285
FONT <- 'Arial'
BODY <- 8
INK <- '#242A30'
FAINT <- '#CCD3D9'
shapes <- c(16,17,15)
cols <- c(CK='#3B5387',PE0.5='#009F86',PVC5='#E54B34')
open_pdf <- function(file,width,height) {
  if(Sys.getenv('FIGURE_PDF_DEVICE','quartz')=='cairo')
    cairo_pdf(file=file,width=width,height=height,family=FONT)
  else quartz(type='pdf',file=file,width=width,height=height,family=FONT)
}
raw_figure <- tempfile(fileext='.pdf')
open_pdf(raw_figure,W/25.4,120/25.4)
grid.newpage()
sw_cache <- new.env(parent=emptyenv())
width_mm <- function(x,size=BODY) vapply(x,function(s) {
  key <- paste(size,s,sep='@')
  if(!exists(key,sw_cache,inherits=FALSE)) assign(key,convertWidth(grobWidth(textGrob(s,gp=gpar(fontfamily=FONT,fontsize=size))),
                                                                  'mm',valueOnly=TRUE),sw_cache)
  get(key,sw_cache,inherits=FALSE)
},numeric(1))
wrap_one <- function(s,lim,size=BODY) {
  if(width_mm(s,size)<=lim) return(s)
  tok <- strsplit(s,'(?<=[ /,;_-])',perl=TRUE)[[1]]
  out <- character()
  cur <- ''
  for(t in tok) {
    if(width_mm(t,size)>lim) {
      # Long database IDs can be broken across display lines; no characters lost.
      for(ch in strsplit(t,'',fixed=TRUE)[[1]]) {
        if(nzchar(cur) && width_mm(paste0(cur,ch),size)>lim) {out<-c(out,trimws(cur));cur<-''}
        cur<-paste0(cur,ch)
      }
    } else if(nzchar(cur) && width_mm(paste0(cur,t),size)>lim) {
      out<-c(out,trimws(cur));cur<-trimws(t,which='left')
    } else cur<-paste0(cur,t)
  }
  paste(c(out,trimws(cur)),collapse='\n')
}
nrline <- function(x) lengths(strsplit(x,'\n',fixed=TRUE))
getComp <- function(type,w) {
  d<-res(paste0('Native_',type,'_composition'))
  cc<-d[Treatment==groups[1]]
  stopifnot(nrow(cc)>0,!anyDuplicated(cc$Feature),all(d$Mean_relative>=0))
  vals<-sapply(groups,function(g) {
    z<-d[Treatment==g]
    stopifnot(identical(cc$Feature,z$Feature),identical(cc$Label,z$Label),identical(cc$Color,z$Color))
    z$Mean_relative
  })
  stopifnot(max(abs(colSums(vals)-100))<1e-8)
  # Pack the legend in contiguous columns, keeping all original annotations.
  col_options <- if(type=='Pathway') 3L else if(type=='Family') 4L else seq_len(min(7L,nrow(cc),max(1L,floor(w/23))))
  candidates<-lapply(col_options,function(nc) {
    cw<-(w-2)/nc
    offset<-0
    if(nrow(cc)==2 && nc==2){
      cw<-max(18,max(width_mm(cc$Label,7.7))+7)
      offset<-13.8+(w-16.2-nc*cw)/2
    }
    wr<-vapply(cc$Label,wrap_one,character(1),lim=cw-4.2,size=7.7)
    hh<-nrline(wr)*3.20+.4
    # Find contiguous cuts that minimize the tallest legend column.
    N<-length(hh); cs<-c(0,cumsum(hh))
    mat<-matrix(Inf,N+1,nc+1); cut<-matrix(0L,N+1,nc+1); mat[1,1]<-0
    for(k in seq_len(nc)) for(j in k:N) {
      js<-(k-1L):(j-1L); score<-pmax(mat[js+1,k],cs[j+1]-cs[js+1]); ix<-which.min(score)
      mat[j+1,k+1]<-score[ix];cut[j+1,k+1]<-js[ix]
    }
    sets<-vector('list',nc);end<-N
    for(k in nc:1){start<-cut[end+1,k+1];sets[[k]]<-(start+1L):end;end<-start}
    list(cw=cw,offset=offset,sets=sets,wr=wr,hh=hh,height=mat[N+1,nc+1],breaks=sum(nrline(wr)-1))
  })
  best<-candidates[[which.min(vapply(candidates,function(q)q$height+.20*q$breaks,numeric(1)))]]
  list(Y=vals,colors=cc$Color,legend=best,h=34+best$height,items=nrow(cc))
}
getLDA <- function(id,w,label_w=NULL) {
  d<-res(paste0('Native_LEfSe_',id))
  stopifnot(nrow(d)>0,all(is.finite(d$LDA)),all(d$LDA>=0),all(d$Treatment%in%groups))
  natural<-max(width_mm(d$Feature,7.8))+3
  lw<-if(is.null(label_w))min(w*.69,max(16,natural))else label_w
  stopifnot(w-lw>15)
  wr<-vapply(d$Feature,wrap_one,character(1),lim=lw-2.4,size=7.8)
  hh<-3.7+3.13*(nrline(wr)-1)
  list(data=d,labels=wr,lw=lw,rh=hh,h=5+sum(hh)+11)
}
pcoa_w<-54
pcoa_h<-41
pcoa_total<-57
venn_h<-42
pc_id <- spec$Panel[spec$Content=='PCoA']
getPC <- function(profile) {
  d<-res(paste0(profile,'_PCoA'))
  z<-res(paste0(profile,'_community'))
  stopifnot(nrow(d)==12,all(table(d$Treatment)==4))
  z[,PCoA1_percent:=PCoA1_percent*(1-Negative_inertia_percent/100)]
  z[,PCoA2_percent:=PCoA2_percent*(1-Negative_inertia_percent/100)]
  p<-pcoa(list(result=z,coords=d));p$layers<-p$layers[-length(p$layers)]
  p<-suppressMessages(p+scale_x_continuous(n.breaks=4,expand=expansion(mult=.08))+
    scale_y_continuous(n.breaks=4,expand=expansion(mult=.075)))+
    theme(text=element_text(family=FONT,size=BODY,colour=INK),
      axis.text=element_text(size=7.5,colour=INK),axis.title=element_text(size=8),
      legend.position='none',plot.margin=margin(0,.7,0,.3,'mm'),
      plot.background=element_blank(),panel.background=element_blank(),
      panel.border=element_rect(fill=NA,colour=INK,linewidth=.29))
  list(z=z,gt=ggplotGrob(p))
}
pc<-setNames(lapply(pc_id,function(id) {
  file<-regmatches(spec[Panel==id,Current_source],regexpr('results/[^;]+_PCoA.csv',spec[Panel==id,Current_source]))
  stopifnot(length(file)==1,nzchar(file))
  getPC(sub('_PCoA.csv$','',basename(file)))
}),pc_id)
if(length(pc)>1){
  ww<-do.call(unit.pmax,lapply(pc,function(q)q$gt$widths))
  hh<-do.call(unit.pmax,lapply(pc,function(q)q$gt$heights))
  for(i in seq_along(pc)){pc[[i]]$gt$widths<-ww;pc[[i]]$gt$heights<-hh}
}
alpha_image<-function(id) {
  pop<-Sys.which('pdftoppm')
  if(!nzchar(pop))pop<-file.path(path.expand('~'),'.cache','codex-runtimes','codex-primary-runtime','dependencies','bin','override','pdftoppm')
  file<-file.path(base,spec[Panel==id,Current_source])
  stopifnot(file.exists(file),file.exists(pop))
  tmp<-tempfile('alpha')
  st<-system2(pop,c('-png','-singlefile','-r','600',shQuote(file),shQuote(tmp)),stdout=FALSE,stderr=FALSE)
  stopifnot(st==0L)
  img<-magick::image_trim(magick::image_read(paste0(tmp,'.png')),fuzz=.5)
  unlink(paste0(tmp,'.png'))
  si<-magick::image_info(img)
  list(img=as.raster(img),ratio=si$width/si$height)
}
panels<-list(); sections<-list(); boundaries<-list()
pan <- function(id,type,x,y,w,height,extra=NULL){
  panels[[id]]<<-list(id=id,type=type,x=x,y=y,w=w,h=height,data=extra)
  height
}
venn<-function(id,x,y,w) {
  d<-res(paste0('Native_Venn_',id))
  stopifnot(identical(as.integer(d$Membership_code),1:7))
  pan(id,'Venn',x,y,w,venn_h,d$Features)
}
ord<-function(id,x,y,w=pcoa_w) pan(id,'PCoA',x,y,w,pcoa_total,pc[[id]])
comp<-function(id,type,x,y,w) {
  d<-getComp(type,w);pan(id,'Composition',x,y,w,d$h,d)
}
lda<-function(id,x,y,w,label_w=NULL) {
  d<-getLDA(id,w,label_w);pan(id,'LEfSe',x,y,w,d$h,d)
}
alpha<-function(id,x,y,w)pan(id,'Original alpha',x,y,w,53,alpha_image(id))
section<-function(title,y,x=4,w=W-8)sections[[length(sections)+1]]<<-list(title=title,x=x,y=y,w=w)
same_top<-function(...) boundaries[[length(boundaries)+1]]<<-list(edge='top',panels=c(...))
simple_row<-function(y,id,ctype) {
  max(venn(id[1],4,y,41),ord(id[2],50,y),comp(id[3],ctype,110,y,171))
}
if(n==11){
  section('MetaCyc pathways',5)
  t<-14
  bot<-max(venn('a',4,t,55)+ord('b',4,t+venn_h,55),
           comp('c','Pathway',65,t,121),lda('d',193,t,88))
  same_top('c','d');H<-t+bot+4
}
if(n==12){
  section('CAZy classes',5)
  t<-14;top_h<-simple_row(t,c('a','b','c'),'Class')
  same_top('b','c')
  y<-t+top_h+7;section('CAZy families',y)
  t<-y+9
  bot<-max(venn('d',4,t,55)+ord('e',4,t+venn_h,55),
           comp('f','Family',65,t,148),lda('g',220,t,61))
  same_top('f','g');H<-t+bot+4
}
if(n==13){
  section('BacMet classes',5)
  t<-14;top_h<-simple_row(t,c('a','b','c'),'Class')
  same_top('b','c')
  y<-t+top_h+7;section('Annotated compounds',y)
  t<-y+9
  lefth<-venn('d',4,t,55)+ord('e',4,t+venn_h,55)
  fh<-comp('f','Compound',65,t,216)
  gh<-lda('g',65,t+fh+1.5,138)
  y<-t+max(lefth,fh+1.5+gh)+7
  section('BacMet genes',y)
  t<-y+9
  bot<-max(venn('h',4,t,55)+ord('i',4,t+venn_h,55),
           comp('j','Gene',65,t,144),lda('k',216,t,65))
  same_top('j','k');H<-t+bot+4
}
if(n==14){
  section('Resistance mechanisms',5,4,117)
  section('Antibiotic classes',5,129,152)
  t<-14
  ah<-ord('a',4,t);bh<-comp('b','Mechanism',63,t,58)
  ch<-ord('c',129,t);dh<-comp('d','Class',188,t,93)
  eh<-lda('e',129,t+ch+1.5,54)
  same_top('a','b','c','d')
  y<-t+max(ah,bh,dh,ch+1.5+eh)+7
  section('Antibiotic resistance ontology terms',y)
  t<-y+9
  lefth<-max(venn('f',4,t,41),ord('g',50,t))
  hh<-comp('h','Gene',4,t+lefth+1.5,150)
  ih<-lda('i',162,t,119,82)
  same_top('g','i')
  H<-t+max(lefth+1.5+hh,ih)+4
}
if(n==15){
  section('Virulence factor profiles',5)
  t<-14
  a<-venn('a',4,t,40)
  b<-alpha('b',47,t,24);c<-alpha('c',74,t,24)
  d<-ord('d',101,t)
  e<-comp('e','Category',162,t,119)
  same_top('d','e')
  t<-t+max(a,b,c,d,e)+4
  f<-comp('f','Gene',4,t,165);g<-lda('g',177,t,104)
  same_top('f','g')
  H<-t+max(f,g)+4
}
stopifnot(setequal(names(panels),spec$Panel))
H<-ceiling(H*10)/10
dev.off();unlink(raw_figure)

measure<-FALSE; records<-list()
txt<-function(s,x,y,size=BODY,just=c('left','centre'),face='plain',colour=INK,rot=0){
  grid.text(s,unit(x,'mm'),unit(H-y,'mm'),just=just,rot=rot,
            gp=gpar(fontfamily=FONT,fontsize=size,fontface=face,col=colour,lineheight=1.01))
}
ln<-function(x1,y1,x2,y2,col=INK,lwd=.60){
  grid.lines(unit(c(x1,x2),'mm'),unit(H-c(y1,y2),'mm'),gp=gpar(col=col,lwd=lwd))
}
rect<-function(x,y,w,h,fill,border=NA,lwd=.35){
  grid.rect(unit(x,'mm'),unit(H-y,'mm'),unit(w,'mm'),unit(h,'mm'),just=c('left','top'),
            gp=gpar(fill=fill,col=border,lwd=lwd))
}
loc<-function(x,y)deviceLoc(unit(x,'mm'),unit(H-y,'mm'),valueOnly=TRUE)
mark<-function(p,x,y,w,h) {
  if(!measure)return(invisible(NULL))
  a<-loc(x,y+h);b<-loc(x+w,y);z<-loc(p$x,p$y)
  records[[p$id]]<<-list(id=p$id,kind=p$type,bbox_pt=72*c(a$x,a$y,b$x,b$y),
                       panel_label_anchor_pt=72*c(z$x,z$y))
}
draw_venn<-function(p) {
  x<-p$x+(p$w-34)/2;y<-p$y-2
  cx<-x+c(12,23,17.5);cy<-y+c(17.5,17.5,27.5)
  for(j in 1:3)grid.circle(unit(cx[j],'mm'),unit(H-cy[j],'mm'),r=unit(10.5,'mm'),
       gp=gpar(fill=adjustcolor(cols[j],alpha.f=.22),col=cols[j],lwd=.70))
  xx<-x+c(7,28,17.5,17.5,11.4,23.6,17.5)
  yy<-y+c(16,16,13.2,32.6,24.2,24.2,20.5)
  for(j in seq_along(xx))txt(format(p$data[j],big.mark=','),xx[j],yy[j],just=c('centre','centre'))
  txt('CK',x+8.2,y+5,size=7.7,just=c('centre','centre'),colour=cols[1])
  txt('PE0.5',x+26.5,y+5,size=7.7,just=c('centre','centre'),colour=cols[2])
  txt('PVC5',x+17.5,y+39.8,size=7.7,just=c('centre','centre'),colour=cols[3])
  mark(p,x+1.5,y+7,32,31)
}
draw_pcoa<-function(p) {
  x<-p$x+(p$w-pcoa_w)/2;y<-p$y+5
  pushViewport(viewport(x=unit(x,'mm'),y=unit(H-y-pcoa_h,'mm'),width=unit(pcoa_w,'mm'),
    height=unit(pcoa_h,'mm'),just=c('left','bottom'),name=paste0('pcoa_',p$id)))
  grid.draw(p$data$gt);upViewport()
  z<-p$data$z
  permanova <- bquote(paste(.(sprintf('PERMANOVA: R² = %.3f, ',z$R2)),italic(P),.(sprintf(' = %.4f',z$P_PERMANOVA))))
  permdisp <- bquote(paste('PERMDISP: ',italic(P),.(sprintf(' = %.3f',z$P_PERMDISP))))
  txt(permanova,p$x+p$w/2,y+pcoa_h+3,7.5,just=c('centre','centre'))
  txt(permdisp,p$x+p$w/2,y+pcoa_h+7,7.5,just=c('centre','centre'))
}
draw_comp<-function(p) {
  s<-p$data;x<-p$x+13.8;y<-p$y+5;pw<-p$w-16.2
  for(j in 1:3){
    yy<-y+(j-1)*5.8;start<-0
    for(i in seq_len(nrow(s$Y))){
      bw<-pw*s$Y[i,j]/100
      if(bw>0)rect(x+start,yy,bw,4.2,s$colors[i])
      start<-start+bw
    }
    ln(x-.80,yy+2.1,x,yy+2.1)
    txt(groups[j],x-1.9,yy+2.1,7.7,just=c('right','centre'))
  }
  ay<-y+17.7;ln(x,ay,x+pw,ay);ln(x,y,x,ay)
  for(v in seq(0,100,25)) {
    xp<-x+pw*v/100
    ln(xp,ay,xp,ay+.8)
    txt(v,xp,ay+2.5,7.5,just=c('centre','centre'))
  }
  txt('Relative abundance (%)',x+pw/2,ay+6.7,8,just=c('centre','centre'))
  st<-s$legend
  for(j in seq_along(st$sets)){
    xx<-p$x+st$offset+(j-1)*st$cw+1
    yy<-p$y+33
    for(i in st$sets[[j]]) {
      rect(xx,yy+.40,2,2,s$colors[i])
      txt(st$wr[i],xx+3.1,yy,7.7,just=c('left','top'))
      yy<-yy+st$hh[i]
    }
  }
  mark(p,x,y,pw,ay-y)
}
draw_lda<-function(p) {
  d<-p$data;x<-p$x+d$lw;y<-p$y+5;pw<-p$w-d$lw-3.2
  lim<-ceiling(max(d$data$LDA)*2)/2
  stopifnot(lim>0)
  offset<-c(0,cumsum(d$rh))
  for(i in seq_len(nrow(d$data))) {
    yy<-y+offset[i]+d$rh[i]/2
    txt(d$labels[i],x-1.7,yy,7.8,just=c('right','centre'))
    rect(x,yy-.97,pw*d$data$LDA[i]/lim,1.94,cols[as.character(d$data$Treatment[i])])
  }
  ay<-y+tail(offset,1)+.8
  ln(x,y,x,ay);ln(x,ay,x+pw,ay)
  for(v in seq(0,lim,1)){
    xp<-x+pw*v/lim;ln(xp,ay,xp,ay+.8)
    txt(v,xp,ay+2.5,7.5,just=c('centre','centre'))
  }
  txt('LDA score',x+pw/2,ay+6.7,8,just=c('centre','centre'))
  mark(p,x,y,pw,ay-y)
}
draw_alpha<-function(p) {
  h<-47;w<-h*p$data$ratio;y<-p$y+5
  if(w>p$w-1){w<-p$w-1;h<-w/p$data$ratio}
  x<-p$x+(p$w-w)/2
  grid.raster(p$data$img,x=unit(x,'mm'),y=unit(H-y,'mm'),width=unit(w,'mm'),
    height=unit(h,'mm'),just=c('left','top'),interpolate=FALSE)
  mark(p,x,y,w,h)
}
draw<-function(audit=FALSE) {
  measure<<-audit;records<<-list();grid.newpage()
  for(s in sections){txt(s$title,s$x,s$y,11,face='bold');ln(s$x,s$y+4,s$x+s$w,s$y+4,FAINT,.68)}
  if(n==14)ln(125,10,125,panels[['g']]$y-13,FAINT,.52)
  for(j in 1:3) {
    x<-W-62+(j-1)*20
    grid.points(unit(x,'mm'),unit(H-5,'mm'),pch=shapes[j],size=unit(2,'mm'),gp=gpar(col=cols[j]))
    txt(groups[j],x+2.4,5,7.8)
  }
  for(p in panels) {
    txt(p$id,p$x,p$y,10.5,face='bold')
    switch(p$type,Venn=draw_venn(p),PCoA=draw_pcoa(p),Composition=draw_comp(p),
      LEfSe=draw_lda(p),'Original alpha'=draw_alpha(p))
  }
  if(audit){
    grid.force()
    vv<-grid.ls(viewports=TRUE,grobs=FALSE,print=FALSE)
    ix<-which(grepl('^panel(?:-[0-9]+)*\\.[0-9]+-[0-9]+-[0-9]+-[0-9]+$',vv$name,perl=TRUE)&grepl('pcoa_',vv$vpPath))
    for(j in ix) {
      tokens<-c(strsplit(vv$vpPath[j],'::',fixed=TRUE)[[1]],vv$name[j]);tokens<-tokens[tokens!='ROOT'&nzchar(tokens)]
      id<-sub('.*pcoa_([a-z]).*','\\1',vv$vpPath[j])
      seekViewport(do.call(vpPath,as.list(tokens)))
      lo<-deviceLoc(unit(0,'npc'),unit(0,'npc'),valueOnly=TRUE);hi<-deviceLoc(unit(1,'npc'),unit(1,'npc'),valueOnly=TRUE)
      upViewport(0);p<-panels[[id]];a<-loc(p$x,p$y)
      records[[id]]<<-list(id=id,kind='PCoA',bbox_pt=72*c(lo$x,lo$y,hi$x,hi$y),panel_label_anchor_pt=72*c(a$x,a$y))
    }
  }
}

probe<-tempfile(fileext='.pdf')
open_pdf(probe,W/25.4,H/25.4);draw(TRUE);dev.off();unlink(probe)
stopifnot(setequal(names(records),spec$Panel))
for(q in panels)stopifnot(q$x>=0,q$x+q$w<=W,q$y>=0,q$y+q$h<=H)
for(group in boundaries){
  top<-vapply(group$panels,function(id)records[[id]]$panel_label_anchor_pt[2],numeric(1))
  stopifnot(diff(range(top))<=1.5)
}
dir.create(dest,recursive=TRUE,showWarnings=FALSE);prefix<-file.path(dest,paste0('FigS',figure_number))
svglite(paste0(prefix,'.svg'),width=W/25.4,height=H/25.4);draw();dev.off()
agg_tiff(paste0(prefix,'.tiff'),width=W,height=H,units='mm',res=600,compression='lzw',background='white');draw();dev.off()
unlink(base,recursive=TRUE)
cat('Exported compact Fig. S',figure_number,'; ',W,' x ',H,' mm.\n',sep='')
