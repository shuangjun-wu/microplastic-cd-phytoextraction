# Fig. S9 | Compact KEGG functional annotation figure.
# Keep FigS9.R and FigS9_data.xlsx together.
# Run: Rscript FigS9.R [--output=/path] [--input=/path.xlsx]
# Uses the saved Venn, PCoA, heatmap and LEfSe source results. No new inference.

resolve_dir <- function() {
  for (frame in rev(sys.frames())) if (!is.null(frame$ofile) && file.exists(frame$ofile))
    return(dirname(normalizePath(frame$ofile)))
  f <- sub('^--file=', '', grep('^--file=', commandArgs(), value=TRUE))
  if (length(f) && file.exists(f[1])) return(dirname(normalizePath(f[1])))
  stop('Run the saved script with Rscript or source its full path.')
}
here <- resolve_dir(); args <- commandArgs(TRUE)
arg <- function(prefix, default) {x<-args[startsWith(args,prefix)];if(length(x))substring(x[1],nchar(prefix)+1)else default}
input <- normalizePath(arg('--input=',file.path(here,'FigS9_data.xlsx')),mustWork=TRUE)
dest <- arg('--output=',here)
packages <- c('readxl','data.table','ggplot2','jsonlite','ragg','svglite')
missing <- packages[!vapply(packages,requireNamespace,logical(1),quietly=TRUE)]
if(length(missing))stop('Missing R packages: ',paste(missing,collapse=', '))
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

specs <- jsonlite::fromJSON(r"---([{"name": "a_通路Venn", "key": "Native_Venn_a", "columns": ["Membership_code", "Features"], "kinds": ["numeric", "numeric"], "n": 7}, {"name": "b_通路PCoA坐标", "key": "KEGG_Level3_RPKM_PCoA", "columns": ["Sample", "Treatment", "Axis1", "Axis2"], "kinds": ["text", "text", "numeric", "numeric"], "n": 12}, {"name": "b_通路群落检验", "key": "KEGG_Level3_RPKM_community", "columns": ["Profile", "Features", "R2", "F", "P_PERMANOVA", "P_PERMDISP", "PCoA1_percent", "PCoA2_percent", "Negative_inertia_percent", "Allocations"], "kinds": ["text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"], "n": 1}, {"name": "c_通路热图", "key": "Native_heatmap_c", "columns": ["Feature", "CK", "PE0.5", "PVC5"], "kinds": ["text", "numeric", "numeric", "numeric"], "n": 50}, {"name": "d_通路LEfSe", "key": "Native_LEfSe_d", "columns": ["Feature", "Treatment", "LDA", "P_original", "Value_source", "Source_PDF", "Original_PDF_LDA", "Row"], "kinds": ["text", "text", "numeric", "numeric", "text", "text", "numeric", "numeric"], "n": 20}, {"name": "e_酶Venn", "key": "Native_Venn_e", "columns": ["Membership_code", "Features"], "kinds": ["numeric", "numeric"], "n": 7}, {"name": "f_酶PCoA坐标", "key": "KEGG_Enzyme_RPKM_PCoA", "columns": ["Sample", "Treatment", "Axis1", "Axis2"], "kinds": ["text", "text", "numeric", "numeric"], "n": 12}, {"name": "f_酶群落检验", "key": "KEGG_Enzyme_RPKM_community", "columns": ["Profile", "Features", "R2", "F", "P_PERMANOVA", "P_PERMDISP", "PCoA1_percent", "PCoA2_percent", "Negative_inertia_percent", "Allocations"], "kinds": ["text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"], "n": 1}, {"name": "g_酶热图", "key": "Native_heatmap_g", "columns": ["Feature", "CK", "PE0.5", "PVC5"], "kinds": ["text", "numeric", "numeric", "numeric"], "n": 50}, {"name": "h_酶LEfSe", "key": "Native_LEfSe_h", "columns": ["Feature", "Treatment", "LDA", "P_original", "Value_source", "Source_PDF", "Original_PDF_LDA", "Row"], "kinds": ["text", "text", "numeric", "numeric", "text", "text", "numeric", "numeric"], "n": 20}, {"name": "g_酶热图排序", "key": "Enzyme_heatmap_display_order", "columns": ["Display_row", "Feature", "Pooled_mean_RPKM", "Previous_row"], "kinds": ["numeric", "text", "numeric", "numeric"], "n": 50}])---",simplifyVector=FALSE)
tables <- list()
for(spec in specs) {
  d <- readxl::read_excel(input,sheet=spec$name,col_types=unlist(spec$kinds),.name_repair='minimal',trim_ws=FALSE)
  if(!identical(names(d),unlist(spec$columns))||nrow(d)!=spec$n) stop('Unexpected worksheet structure: ',spec$name)
  tables[[spec$key]] <- as.data.table(d)
}
res <- function(name) copy(tables[[name]])
section <- function(prefix, ids, title) {
  m <- res(paste0('Native_heatmap_',ids[3]))
  if (ids[3]=='g') {
    # The three treatment means each represent four pots; pool on the RPKM scale.
    pooled <- rowMeans(10^as.matrix(m[,..groups]))
    idx <- order(-pooled, seq_along(pooled))
    m <- m[idx]
    shown <- res('Enzyme_heatmap_display_order')
    stopifnot(identical(as.character(m$Feature),as.character(shown$Feature)),
              all(shown$Display_row==seq_len(nrow(shown))),all(shown$Previous_row==idx))
  }
  Y <- as.matrix(m[,..groups])
  d <- res(paste0(prefix,'_PCoA'))
  q <- res(paste0(prefix,'_community'))
  v <- res(paste0('Native_Venn_',ids[1]))
  l <- res(paste0('Native_LEfSe_',ids[4]))
  stopifnot(nrow(Y)==50, ncol(Y)==3, all(is.finite(Y)), !anyDuplicated(m$Feature),
            nrow(l)==20, all(l$LDA>=0), nrow(d)==12, all(table(d$Treatment)==4),
            identical(as.integer(v$Membership_code),1:7))
  q[,PCoA1_percent:=PCoA1_percent*(1-Negative_inertia_percent/100)]
  q[,PCoA2_percent:=PCoA2_percent*(1-Negative_inertia_percent/100)]
  list(ids=ids, title=title, heat=list(labels=m$Feature,Y=Y), lda=l, venn=v$Features,
       ord=list(result=q,coords=d))
}
ss <- list(section('KEGG_Level3_RPKM',letters[1:4],'KEGG Level 3 pathways'),
           section('KEGG_Enzyme_RPKM',letters[5:8],'KEGG enzymes'))
FONT <- 'Arial'
open_pdf <- function(file,width,height) {
  if (Sys.getenv('FIGURE_PDF_DEVICE','quartz')=='cairo')
    cairo_pdf(file=file,width=width,height=height,family=FONT)
  else quartz(type='pdf',file=file,width=width,height=height,family=FONT)
}
BODY <- 8
INK <- '#242A30'
FAINT <- '#CCD3D9'
COL <- cols
SHAPE <- c(16,17,15)
row_mm <- 3.30
heat_top <- 23.0
heat_h <- 50*row_mm
tile_mm <- 8.4
tile_w <- 3*tile_mm
H <- 204
tmp <- tempfile(fileext='.pdf')
open_pdf(tmp,10,H/25.4)
grid.newpage()
strwidth_mm <- function(x,size=BODY) {
  if(!length(x)) return(numeric())
  vapply(x,function(z) convertWidth(grobWidth(textGrob(z,gp=gpar(fontfamily=FONT,fontsize=size))),'mm',valueOnly=TRUE),numeric(1))
}
wrap_label <- function(s,maxw) {
  if(strwidth_mm(s)<=maxw) return(s)
  tok <- strsplit(s,' ',fixed=TRUE)[[1]]
  a <- character(); current <- ''
  for(t in tok) {
    cand <- if(nzchar(current)) paste(current,t) else t
    if(nzchar(current) && strwidth_mm(cand)>maxw) {a<-c(a,current);current<-t} else current<-cand
  }
  paste(c(a,current),collapse='\n')
}
support_w <- c(82,51)
for (i in seq_along(ss)) {
  z <- ss[[i]]
  z$lab_w <- ceiling(max(strwidth_mm(z$heat$labels)))+2
  z$heat_w <- z$lab_w+tile_w+2
  z$support_w <- support_w[i]
  z$lda_lab_w <- if(i==1) 56 else 19
  z$lda_labels <- vapply(z$lda$Feature,wrap_label,character(1),maxw=z$lda_lab_w-2.3)
  z$lda_lines <- lengths(strsplit(z$lda_labels,'\n',fixed=TRUE))
  stopifnot(all(strwidth_mm(unlist(strsplit(z$lda_labels,'\n',fixed=TRUE)))<=z$lda_lab_w-2.3))
  ss[[i]] <- z
}

M <- 4
colgap <- 3.5
sectiongap <- 6
secw <- vapply(ss,function(z) z$support_w+colgap+z$heat_w,numeric(1))
xleft <- c(M,M+secw[1]+sectiongap)
W <- 2*M+sum(secw)+sectiongap
measure <- FALSE
records <- list()
txt <- function(s,x,y,size=BODY,just=c('left','centre'),face='plain',colour=INK,rot=0) {
  grid.text(s,unit(x,'mm'),unit(H-y,'mm'),just=just,rot=rot,
            gp=gpar(fontfamily=FONT,fontsize=size,fontface=face,col=colour,lineheight=1.05))
}
ln <- function(x1,y1,x2,y2,col=INK,lwd=.60) {
  grid.lines(unit(c(x1,x2),'mm'),unit(H-c(y1,y2),'mm'),gp=gpar(col=col,lwd=lwd))
}
rect <- function(x,y,w,h,fill,border=NA,lwd=.35) {
  grid.rect(unit(x,'mm'),unit(H-y,'mm'),unit(w,'mm'),unit(h,'mm'),
            just=c('left','top'),gp=gpar(fill=fill,col=border,lwd=lwd))
}
pt <- function(x,y,shape,col,size) grid.points(unit(x,'mm'),unit(H-y,'mm'),pch=shape,
          size=unit(size,'mm'),gp=gpar(col=col))
mark <- function(id,x,y,w,h,kind) {
  if(!measure) return(invisible(NULL))
  a <- deviceLoc(unit(x,'mm'),unit(H-y-h,'mm'),valueOnly=TRUE)
  b <- deviceLoc(unit(x+w,'mm'),unit(H-y,'mm'),valueOnly=TRUE)
  records[[length(records)+1L]] <<- list(id=id, kind=kind,bbox_pt=72*c(a$x,a$y,b$x,b$y))
}
panel_tag <- function(id,x,y) txt(id,x,y,10.5,face='bold')
toppanel <- 14
pcoa_y <- 59
pcoa_w <- 50
pcoa_h <- 42
lda_y <- 115
lda_end <- 190
palette <- colorRampPalette(c('#3973A6','#B9D4CE','#FAF2B0','#EC8C42','#C6312C'))(256)

draw_venn <- function(s,x) {
  panel_tag(s$ids[1],x,toppanel)
  # Triangular Venn in a compact centered region; preserves all seven counts.
  left <- x+(s$support_w-34)/2
  cy <- c(30,30,40)
  cx <- left+c(12,23,17.5)
  for (j in seq_len(3))
    grid.circle(unit(cx[j],'mm'),unit(H-cy[j],'mm'),r=unit(10.5,'mm'),
                gp=gpar(fill=adjustcolor(COL[j],alpha.f=.22),col=COL[j],lwd=.7))
  xx <- left+c(7,28,17.5,17.5,11.4,23.6,17.5)
  yy <- c(28.5,28.5,25.7,45.1,36.7,36.7,33.0)
  for(j in seq_along(xx)) txt(format(s$venn[j],big.mark=','),xx[j],yy[j],just=c('centre','centre'))
  txt('CK',left+8.2,17.5,size=7.7,just=c('centre','centre'),colour=COL[1])
  txt('PE0.5',left+26.5,17.5,size=7.7,just=c('centre','centre'),colour=COL[2])
  txt('PVC5',left+17.5,52.3,size=7.7,just=c('centre','centre'),colour=COL[3])
}
pc <- lapply(ss,function(s) {
  p <- pcoa(s$ord)
  p$layers <- p$layers[-length(p$layers)]
  p <- suppressMessages(p+scale_x_continuous(n.breaks=4,expand=expansion(mult=.08))+
       scale_y_continuous(n.breaks=4,expand=expansion(mult=.075)))+
       theme(text=element_text(family=FONT,size=BODY,colour=INK),
       axis.text=element_text(size=7.4,colour=INK),axis.title=element_text(size=8.1),
       legend.position='none',plot.margin=margin(.3,.6,0,.3,'mm'),
       plot.background=element_blank(),panel.background=element_blank(),
       panel.border=element_rect(fill=NA,colour=INK,linewidth=.29))
  ggplotGrob(p)
})
same_widths <- do.call(unit.pmax,lapply(pc,function(x)x$widths))
same_heights <- do.call(unit.pmax,lapply(pc,function(x)x$heights))
for(i in seq_along(pc)) {pc[[i]]$widths<-same_widths;pc[[i]]$heights<-same_heights}
dev.off()
unlink(tmp)

draw_pcoa <- function(s,x,i) {
  panel_tag(s$ids[2],x,55.5)
  px <- x+(s$support_w-pcoa_w)/2
  pushViewport(viewport(x=unit(px,'mm'),y=unit(H-pcoa_y-pcoa_h,'mm'),
                  width=unit(pcoa_w,'mm'),height=unit(pcoa_h,'mm'),
                  just=c('left','bottom'),name=paste0('pcoa_',s$ids[2])))
  grid.draw(pc[[i]])
  upViewport()
  z <- s$ord$result
  p_permanova<-bquote(paste(.(sprintf('PERMANOVA: R² = %.3f, ',z$R2)),italic(P),.(sprintf(' = %.4f',z$P_PERMANOVA))))
  p_permdisp<-bquote(paste('PERMDISP: ',italic(P),.(sprintf(' = %.3f',z$P_PERMDISP))))
  txt(p_permanova,x+s$support_w/2,104,7.5,just=c('centre','centre'))
  txt(p_permdisp,x+s$support_w/2,107.9,7.5,just=c('centre','centre'))
}
draw_lda <- function(s,x) {
  panel_tag(s$ids[4],x,112.7)
  z <- s$lda
  x0 <- x+s$lda_lab_w
  span <- s$support_w-s$lda_lab_w-3.3
  # Both LDA axes have the same origin and limits; only physical widths differ.
  lim <- ceiling(max(vapply(ss,function(q)max(q$lda$LDA),numeric(1)))*2)/2
  edge <- lda_y+c(0,cumsum(s$lda_lines+0.20))/sum(s$lda_lines+0.20)*(lda_end-lda_y-1.5)
  yy <- (head(edge,-1)+tail(edge,-1))/2
  for(j in seq_len(nrow(z))) {
    txt(s$lda_labels[j],x0-1.7,yy[j],7.8,just=c('right','centre'))
    rect(x0,yy[j]-.96,span*z$LDA[j]/lim,1.92,COL[as.character(z$Treatment[j])])
  }
  ln(x0,lda_end,x0+span,lda_end)
  ln(x0,lda_y,x0,lda_end)
  for(v in seq(0,lim,1)) {
    xp<-x0+span*v/lim
    ln(xp,lda_end,xp,lda_end+.85)
    txt(as.character(v),xp,lda_end+2.7,7.6,just=c('centre','centre'))
  }
  txt('LDA score',x0+span/2,lda_end+6.8,8.1,just=c('centre','centre'))
}
draw_heat <- function(s,x) {
  panel_tag(s$ids[3],x,toppanel)
  x0 <- x+s$lab_w
  Y <- s$heat$Y
  lim <- range(Y)
  stopifnot(diff(lim)>0)
  col_index <- matrix(pmin(256L,1L+floor((Y-lim[1])/diff(lim)*255)),nrow(Y))
  for(j in seq_len(nrow(Y))) {
    yy <- heat_top+(j-1)*row_mm
    txt(s$heat$labels[j],x0-1.8,yy+row_mm/2,just=c('right','centre'))
    for(g in seq_along(groups))
      rect(x0+(g-1)*tile_mm,yy,tile_mm,row_mm,palette[col_index[j,g]],'#FFFFFF',.33)
  }
  for(g in seq_along(groups)) {
    xx<-x0+(g-.5)*tile_mm
    txt(groups[g],xx,20,7.7,just=c('centre','centre'))
  }
  mark(s$ids[3],x0,heat_top,tile_w,heat_h,'Heatmap')
  cy <- heat_top+heat_h+3.8
  for(k in seq_along(palette)) rect(x0+(k-1)*tile_w/256,cy,tile_w/256+.01,2,palette[k])
  ticks <- pretty(lim,3)
  ticks <- ticks[ticks>=lim[1] & ticks<=lim[2]]
  for(v in ticks) {
    xp<-x0+tile_w*(v-lim[1])/diff(lim)
    ln(xp,cy+2,xp,cy+2.65,col=INK,lwd=.45)
    txt(format(v,trim=TRUE),xp,cy+4.15,7.5,just=c('centre','centre'))
  }
  txt('log₁₀ RPKM',x0+tile_w/2,cy+7.6,7.5,just=c('centre','centre'))
}
draw <- function(audit=FALSE) {
  measure <<- audit
  records <<- list()
  grid.newpage()
  sep <- xleft[1]+secw[1]+sectiongap/2
  ln(sep,10,sep,H-4,FAINT,.52)
  for(i in seq_along(ss)) {
    s <- ss[[i]]
    x <- xleft[i]
    txt(s$title,x,5,11,face='bold')
    ln(x,9,x+secw[i],9,FAINT,.68)
    draw_venn(s,x)
    draw_pcoa(s,x,i)
    draw_lda(s,x)
    draw_heat(s,x+s$support_w+colgap)
  }
  for (i in seq_along(groups)) {
    x <- W-62+(i-1)*20
    pt(x,5,SHAPE[i],COL[i],2.0)
    txt(groups[i],x+2.4,5,7.8)
  }
  if(audit) {
    grid.force()
    vv<-grid.ls(viewports=TRUE,grobs=FALSE,print=FALSE)
    ix<-which(grepl('^panel(?:-[0-9]+)*\\.[0-9]+-[0-9]+-[0-9]+-[0-9]+$',vv$name,perl=TRUE)&grepl('pcoa_',vv$vpPath))
    for(j in ix) {
      tokens<-c(strsplit(vv$vpPath[j],'::',fixed=TRUE)[[1]],vv$name[j])
      tokens<-tokens[tokens!='ROOT'&nzchar(tokens)]
      seekViewport(do.call(vpPath,as.list(tokens)))
      a<-deviceLoc(unit(0,'npc'),unit(0,'npc'),valueOnly=TRUE)
      b<-deviceLoc(unit(1,'npc'),unit(1,'npc'),valueOnly=TRUE)
      records[[length(records)+1L]]<<-list(id=sub('.*pcoa_([a-z]).*','\\1',vv$vpPath[j]),kind='PCoA',
                                           bbox_pt=72*c(a$x,a$y,b$x,b$y))
      upViewport(0)
    }
  }
}

# Check actual panel alignment before export, without external scripts or output logs.
probe <- tempfile(fileext='.pdf')
open_pdf(probe,W/25.4,H/25.4)
draw(TRUE);dev.off();unlink(probe)
stopifnot(setequal(vapply(records,function(z)z$id,character(1)),c('b','f','c','g')))
boxes <- setNames(lapply(records,`[[`,'bbox_pt'),vapply(records,`[[`,character(1),'id'))
for(pair in list(c('b','f'),c('c','g'))) {
  p<-boxes[[pair[1]]];q<-boxes[[pair[2]]]
  stopifnot(max(abs(p[c(2,4)]-q[c(2,4)]))<=1.5,abs(diff(p[c(1,3)])-diff(q[c(1,3)]))<=1.5)
}
dir.create(dest,recursive=TRUE,showWarnings=FALSE)
prefix <- file.path(dest,'FigS9')
svglite(paste0(prefix,'.svg'),width=W/25.4,height=H/25.4)
draw();dev.off()
agg_tiff(paste0(prefix,'.tiff'),width=W,height=H,units='mm',res=600,compression='lzw',background='white')
draw();dev.off()
message('Exported the compact Fig. S9 as TIFF and SVG: ',normalizePath(dest))
