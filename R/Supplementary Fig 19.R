# Fig. S19 整理与图稿导出入口
# 本目录原稿由平台 PDF + Adobe Illustrator 手工排版生成；未找到原始富集分析脚本。
# 本脚本读取现有 XLSX，检查来源，导出 AI 第 1 画板为 SVG；不重新计算富集统计。
# SVG 文字转为矢量轮廓，编辑原文字请打开 AI；TIFF 保持原稿 300 dpi 和原文件内容。
# 用法：Rscript FigS19.R --check-only
#       Rscript FigS19.R [--output=/目标文件夹] [--python=/Python路径]
# Python 环境需安装 PyMuPDF（pymupdf）；R 需 readxl、digest。

args <- commandArgs(trailingOnly=TRUE)
arg <- function(prefix, default) {
  x <- args[startsWith(args,prefix)]
  if(length(x)) substring(x[1],nchar(prefix)+1) else default
}
full <- commandArgs(trailingOnly=FALSE)
script <- sub('^--file=','',full[startsWith(full,'--file=')][1])
here <- if(!is.na(script)) dirname(normalizePath(script)) else getwd()
for(p in c('readxl','digest')) if(!requireNamespace(p,quietly=TRUE)) stop('缺少 R 包：',p)
input <- normalizePath(arg('--input=',file.path(here,'FigS19_data.xlsx')),mustWork=TRUE)
sheets <- readxl::excel_sheets(input)
data <- setNames(lapply(sheets,function(s) as.data.frame(readxl::read_excel(input,sheet=s,.name_repair='minimal'))),sheets)
stopifnot(all(c('图与数据说明','来源文件','原图文字记录') %in% names(data)))
for(s in sheets) cat(s,': ',nrow(data[[s]]),' 行\n',sep='')
cat('注意：图稿文字记录是可见文字提取，不是完整的原始富集结果。\n')
ai <- file.path(here,'FigS19.ai')
tif <- file.path(here,'FigS19.tiff')
stopifnot(file.exists(ai),file.exists(tif))
expected <- c('19cfd212d06eda4a6ca736a1c478563fee96f01723c6e8994db074adaed3f0ce','2bde3a3d53c02acb17117f0740d668cc3d7dfecd7dbbca5abd96e83d3bf5dee9')
actual <- vapply(c(ai,tif),function(p) digest::digest(file=p,algo='sha256'),character(1))
if(!identical(unname(actual),expected)) stop('AI/TIFF 已发生变化。请核对母版与 TIFF 是否仍对应，再更新导出流程。')
cat('原 AI 和 TIFF 与整理前校验值一致。\n')
if('--check-only' %in% args) quit(status=0)
out <- arg('--output=',here);dir.create(out,recursive=TRUE,showWarnings=FALSE)
out <- normalizePath(out)
python <- arg('--python=',Sys.getenv('FIGURE_PYTHON',unset=Sys.which('python3')))
if(!nzchar(python)) stop('请通过 --python= 指定已安装 PyMuPDF 的 Python。')
py <- tempfile(fileext='.py')
writeLines(c(
  'import sys',
  'from pathlib import Path',
  'import pymupdf',
  'doc = pymupdf.open(sys.argv[1])',
  'svg = doc[0].get_svg_image(text_as_path=True)',
  r"----(svg = svg.replace('>', '><rect width="100%" height="100%" fill="white"/>', 1))----",
  'Path(sys.argv[2]).write_text(svg, encoding="utf-8")'
),py,useBytes=TRUE)
status <- system2(python,c(shQuote(py),shQuote(ai),shQuote(file.path(out,'FigS19.svg'))))
unlink(py)
if(status!=0) stop('SVG 导出失败；请检查 Python 中是否可 import pymupdf。')
if(out!=normalizePath(here)){
  stopifnot(file.copy(tif,file.path(out,basename(tif)),overwrite=TRUE))
}
cat('完成：SVG 为矢量轮廓，AI 保留原可编辑对象；TIFF 沿用原文件。\n')
