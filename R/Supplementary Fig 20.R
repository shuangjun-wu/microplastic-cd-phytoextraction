# Fig. S20: complete biomass-associated GO network, NES-ranked representatives.
# 功能簇：与 fig1 相同的单文件组织形式。
# 本文件与 FigS20_data.xlsx 放在同一目录。图稿只导出 TIFF/SVG。
# 使用：Rscript FigS20.R [--input=数据.xlsx] [--output=输出目录] [--check-only]
# 默认只重绘已确认的完整网络 NES 版；沿用原 GSEA 数值与已保存固定坐标。
# --mode=complete-nes：仅重绘完整网络 NES 版，沿用全部保存结果。
# --mode=rebuild-preview：从显著 GO 与匹配基因集重建 Python MCL 试排，再绘图。
# --mode=cytoscape：仅在已开启 Cytoscape 并安装 EnrichmentMap/AutoAnnotate 时重建该网络。
# 原分析使用 Python；以下将原代码嵌入同一个 R 入口，保留其算法、参数和绘图样式。
# Python 绘图依赖：numpy, pandas, networkx, matplotlib, Pillow。
# MCL 重建另需 scipy, markov_clustering；可用 --python=指定已有解释器。
# Excel 中可直接阅读和编辑的表格是 CSV 与 GO 基因集的输入来源；原始工程以压缩块保留。

resolve_dir <- function() {
  for (fr in rev(sys.frames())) if (!is.null(fr$ofile) && file.exists(fr$ofile)) return(dirname(normalizePath(fr$ofile)))
  ff <- sub('^--file=', '', grep('^--file=', commandArgs(), value=TRUE))
  if (length(ff) && file.exists(ff[1])) return(dirname(normalizePath(ff[1])))
  if (requireNamespace('rstudioapi', quietly=TRUE) && rstudioapi::isAvailable()) {
    ff <- rstudioapi::getActiveDocumentContext()$path
    if (nzchar(ff) && file.exists(ff)) return(dirname(normalizePath(ff)))
  }
  stop('Please run the saved script with Rscript or source its full path.')
}

run_functional_clusters <- function() {
  script_dir <- resolve_dir(); args <- commandArgs(trailingOnly=TRUE)
  arg <- function(key, default) { x <- args[startsWith(args,key)]; if(length(x)) substring(x[1],nchar(key)+1) else default }
  input <- arg('--input=', file.path(script_dir,'FigS20_data.xlsx'))
  output <- arg('--output=',script_dir)
  mode <- arg('--mode=','complete-nes')
  if (!mode %in% c('plot','rebuild-preview','cytoscape','complete-nes')) stop('Unknown --mode: ',mode)
  packages <- c('readxl','jsonlite','base64enc','digest')
  missing <- packages[!vapply(packages,requireNamespace,logical(1),quietly=TRUE)]
  if (length(missing)) stop('Missing R packages: ',paste(missing,collapse=', '))
  if (!file.exists(input)) stop('Workbook not found: ',input)
  input <- normalizePath(input); original_hash <- tools::md5sum(input)
  work_arg<-arg('--work-dir=','')
  stage<-if(nzchar(work_arg))work_arg else tempfile('functional_clusters_')
  dir.create(stage,recursive=TRUE,showWarnings=FALSE)
  if(!nzchar(work_arg))on.exit(unlink(stage,recursive=TRUE),add=TRUE)
  for (folder in c('data','code','figure')) dir.create(file.path(stage,folder),showWarnings=FALSE)
  specs <- jsonlite::fromJSON(r"----([{"name": "原始数据", "columns": ["GO", "pval", "padj", "log2err", "ES", "NES", "size", "leadingEdge", "BH_adjusted_P", "Analysis", "Tissue", "Sample_subset", "Namespace", "Description", "Significant", "Direction", "Leading_edge_n", "Leading_edge_positive_coefficient_fraction", "Leading_edge_median_Pearson_r", "Set_positive_coefficient_fraction", "Set_median_Pearson_r"], "kinds": ["string", "number", "number", "number", "number", "number", "number", "string", "number", "string", "string", "string", "string", "string", "boolean", "string", "number", "number", "number", "number", "number"], "target": "Functional_cluster_preview/data/significant_BP_original.csv", "rows": 1563}, {"name": "运算数据", "columns": ["GO", "pval", "padj", "log2err", "ES", "NES", "size", "leadingEdge", "BH_adjusted_P", "Analysis", "Tissue", "Sample_subset", "Namespace", "Description", "Significant", "Direction", "Leading_edge_n", "Leading_edge_positive_coefficient_fraction", "Leading_edge_median_Pearson_r", "Set_positive_coefficient_fraction", "Set_median_Pearson_r", "Cluster", "Displayed_cluster"], "kinds": ["string", "number", "number", "number", "number", "number", "number", "string", "number", "string", "string", "string", "string", "string", "boolean", "string", "number", "number", "number", "number", "number", "string", "boolean"], "target": "Functional_cluster_preview/results/all_significant_nodes_and_clusters.csv", "rows": 1563}, {"name": "汇总网络_输入边", "columns": ["Analysis", "source", "target", "Combined", "Shared_genes"], "kinds": ["string", "string", "string", "number", "number"], "target": "EnrichmentMap_AutoAnnotate/Summary_network_example/data/all_edges.csv", "rows": 1908}, {"name": "汇总网络_输入节点", "columns": ["GO", "pval", "padj", "log2err", "ES", "NES", "size", "leadingEdge", "BH_adjusted_P", "Analysis", "Tissue", "Sample_subset", "Namespace", "Description", "Significant", "Direction", "Leading_edge_n", "Leading_edge_positive_coefficient_fraction", "Leading_edge_median_Pearson_r", "Set_positive_coefficient_fraction", "Set_median_Pearson_r", "Cluster"], "kinds": ["string", "number", "number", "number", "number", "number", "number", "string", "number", "string", "string", "string", "string", "string", "boolean", "string", "number", "number", "number", "number", "number", "string"], "target": "EnrichmentMap_AutoAnnotate/Summary_network_example/data/all_nodes.csv", "rows": 484}, {"name": "汇总网络_输入功能簇", "columns": ["Analysis", "Cluster", "Label", "Terms", "Positive", "Negative", "GO_members"], "kinds": ["string", "string", "string", "number", "number", "number", "string"], "target": "EnrichmentMap_AutoAnnotate/Summary_network_example/data/clusters.csv", "rows": 101}, {"name": "汇总网络_代表条目", "columns": ["Analysis", "Tissue", "Sample_subset", "GO", "Description", "NES", "Original_P", "Original_BH_P", "size", "Leading_edge_n", "NES_direction", "Retained", "Direct_representative_GO", "Mapping_chain", "Mapping_depth", "Representative_GO", "Representative_description", "Representative_NES", "Shared_genes_with_representative", "Jaccard_with_representative", "abs_NES", "Display_rank", "Sign", "Cluster", "Term_number", "Display_cluster"], "kinds": ["string", "string", "string", "string", "string", "number", "number", "number", "number", "number", "string", "boolean", "string", "string", "number", "string", "string", "number", "number", "number", "number", "number", "string", "string", "number", "string"], "target": "EnrichmentMap_AutoAnnotate/Summary_network_example/data/displayed_terms_with_clusters.csv", "rows": 10}, {"name": "汇总网络_覆盖统计", "columns": ["Display", "Clusters", "GO_terms", "GO_coverage"], "kinds": ["string", "number", "number", "number"], "target": "EnrichmentMap_AutoAnnotate/Summary_network_example/results/coverage_comparison.csv", "rows": 2}, {"name": "汇总网络_固定坐标", "columns": ["Cluster", "x", "y", "radius"], "kinds": ["string", "number", "number", "number"], "target": "EnrichmentMap_AutoAnnotate/Summary_network_example/results/fixed_summary_coordinates.csv", "rows": 101}, {"name": "汇总网络_计算边", "columns": ["source", "target", "GO_pair_count"], "kinds": ["string", "string", "number"], "target": "EnrichmentMap_AutoAnnotate/Summary_network_example/results/summary_edges.csv", "rows": 53}, {"name": "汇总网络_计算节点", "columns": ["Analysis", "Cluster", "Label", "Terms", "Positive", "Negative", "GO_members", "Selected_by_upper_table", "Positive_fraction"], "kinds": ["string", "string", "string", "number", "number", "number", "string", "boolean", "number"], "target": "EnrichmentMap_AutoAnnotate/Summary_network_example/results/summary_nodes.csv", "rows": 101}, {"name": "原始_去冗余代表条目", "columns": ["Analysis", "Tissue", "Sample_subset", "GO", "Description", "NES", "Original_P", "Original_BH_P", "size", "Leading_edge_n", "NES_direction", "Retained", "Direct_representative_GO", "Mapping_chain", "Mapping_depth", "Representative_GO", "Representative_description", "Representative_NES", "Shared_genes_with_representative", "Jaccard_with_representative"], "kinds": ["string", "string", "string", "string", "string", "number", "number", "number", "number", "number", "string", "boolean", "string", "string", "number", "string", "string", "number", "number", "number"], "target": "EnrichmentMap_AutoAnnotate/data/GO_BP_retained_representatives.csv", "rows": 527}, {"name": "原始数据", "columns": ["GO", "pval", "padj", "log2err", "ES", "NES", "size", "leadingEdge", "BH_adjusted_P", "Analysis", "Tissue", "Sample_subset", "Namespace", "Description", "Significant", "Direction", "Leading_edge_n", "Leading_edge_positive_coefficient_fraction", "Leading_edge_median_Pearson_r", "Set_positive_coefficient_fraction", "Set_median_Pearson_r"], "kinds": ["string", "number", "number", "number", "number", "number", "number", "string", "number", "string", "string", "string", "string", "string", "boolean", "string", "number", "number", "number", "number", "number"], "target": "EnrichmentMap_AutoAnnotate/data/significant_BP_original.csv", "rows": 1563}, {"name": "P与NES_显示范围", "columns": ["Analysis", "Significant_terms", "All_clusters", "NES_displayed_clusters", "NES_displayed_GO", "P_displayed_clusters", "P_displayed_GO"], "kinds": ["string", "number", "number", "number", "number", "number", "number"], "target": "EnrichmentMap_AutoAnnotate/results/P_vs_NES_display_scope.csv", "rows": 4}, {"name": "P与NES_选择对照", "columns": ["Analysis", "Shared_terms", "NES_only", "P_only"], "kinds": ["string", "number", "string", "string"], "target": "EnrichmentMap_AutoAnnotate/results/P_vs_NES_selection_comparison.csv", "rows": 4}, {"name": "AutoAnnotate_完整边表", "columns": ["Analysis", "source", "target", "Combined", "Shared_genes"], "kinds": ["string", "string", "string", "number", "number"], "target": "EnrichmentMap_AutoAnnotate/results/all_edges.csv", "rows": 5566}, {"name": "AutoAnnotate_完整节点", "columns": ["GO", "pval", "padj", "log2err", "ES", "NES", "size", "leadingEdge", "BH_adjusted_P", "Analysis", "Tissue", "Sample_subset", "Namespace", "Description", "Significant", "Direction", "Leading_edge_n", "Leading_edge_positive_coefficient_fraction", "Leading_edge_median_Pearson_r", "Set_positive_coefficient_fraction", "Set_median_Pearson_r", "Cluster"], "kinds": ["string", "number", "number", "number", "number", "number", "number", "string", "number", "string", "string", "string", "string", "string", "boolean", "string", "number", "number", "number", "number", "number", "string"], "target": "EnrichmentMap_AutoAnnotate/results/all_nodes.csv", "rows": 1563}, {"name": "AutoAnnotate_功能簇", "columns": ["Analysis", "Cluster", "Label", "Terms", "Positive", "Negative", "GO_members"], "kinds": ["string", "string", "string", "number", "number", "number", "string"], "target": "EnrichmentMap_AutoAnnotate/results/clusters.csv", "rows": 366}, {"name": "NES版_节点坐标", "columns": ["Analysis", "GO", "Cluster", "x", "y"], "kinds": ["string", "string", "string", "number", "number"], "target": "EnrichmentMap_AutoAnnotate/results/display_coordinates.csv", "rows": 225}, {"name": "P值版_节点坐标", "columns": ["Analysis", "GO", "Cluster", "x", "y"], "kinds": ["string", "string", "string", "number", "number"], "target": "EnrichmentMap_AutoAnnotate/results/display_coordinates_P.csv", "rows": 283}, {"name": "NES版_显示统计", "columns": ["Analysis", "Significant_terms", "Retained_terms", "Displayed_terms", "All_clusters", "Displayed_clusters", "Terms_in_displayed_clusters", "Displayed_cluster_GO_coverage"], "kinds": ["string", "number", "number", "number", "number", "number", "number", "number"], "target": "EnrichmentMap_AutoAnnotate/results/display_summary.csv", "rows": 4}, {"name": "P值版_显示统计", "columns": ["Analysis", "Significant_terms", "Retained_terms", "Displayed_terms", "All_clusters", "Displayed_clusters", "Terms_in_displayed_clusters", "Displayed_cluster_GO_coverage"], "kinds": ["string", "number", "number", "number", "number", "number", "number", "number"], "target": "EnrichmentMap_AutoAnnotate/results/display_summary_P.csv", "rows": 4}, {"name": "NES版_选择条目", "columns": ["Analysis", "Tissue", "Sample_subset", "GO", "Description", "NES", "Original_P", "Original_BH_P", "size", "Leading_edge_n", "NES_direction", "Retained", "Direct_representative_GO", "Mapping_chain", "Mapping_depth", "Representative_GO", "Representative_description", "Representative_NES", "Shared_genes_with_representative", "Jaccard_with_representative", "abs_NES", "Display_rank", "Sign", "Cluster"], "kinds": ["string", "string", "string", "string", "string", "number", "number", "number", "number", "number", "string", "boolean", "string", "string", "number", "string", "string", "number", "number", "number", "number", "number", "string", "string"], "target": "EnrichmentMap_AutoAnnotate/results/displayed_terms.csv", "rows": 40}, {"name": "P值版_选择条目", "columns": ["Analysis", "Tissue", "Sample_subset", "GO", "Description", "NES", "Original_P", "Original_BH_P", "size", "Leading_edge_n", "NES_direction", "Retained", "Direct_representative_GO", "Mapping_chain", "Mapping_depth", "Representative_GO", "Representative_description", "Representative_NES", "Shared_genes_with_representative", "Jaccard_with_representative", "abs_NES", "Display_rank", "Sign", "Cluster"], "kinds": ["string", "string", "string", "string", "string", "number", "number", "number", "number", "number", "string", "boolean", "string", "string", "number", "string", "string", "number", "number", "number", "number", "number", "string", "string"], "target": "EnrichmentMap_AutoAnnotate/results/displayed_terms_P.csv", "rows": 40}, {"name": "P值版_绘图条目", "columns": ["Analysis", "Tissue", "Sample_subset", "GO", "Description", "NES", "Original_P", "Original_BH_P", "size", "Leading_edge_n", "NES_direction", "Retained", "Direct_representative_GO", "Mapping_chain", "Mapping_depth", "Representative_GO", "Representative_description", "Representative_NES", "Shared_genes_with_representative", "Jaccard_with_representative", "abs_NES", "Display_rank", "Sign", "Cluster", "Term_number", "Display_cluster"], "kinds": ["string", "string", "string", "string", "string", "number", "number", "number", "number", "number", "string", "boolean", "string", "string", "number", "string", "string", "number", "number", "number", "number", "number", "string", "string", "number", "string"], "target": "EnrichmentMap_AutoAnnotate/results/displayed_terms_P_with_clusters.csv", "rows": 40}, {"name": "NES版_绘图条目", "columns": ["Analysis", "Tissue", "Sample_subset", "GO", "Description", "NES", "Original_P", "Original_BH_P", "size", "Leading_edge_n", "NES_direction", "Retained", "Direct_representative_GO", "Mapping_chain", "Mapping_depth", "Representative_GO", "Representative_description", "Representative_NES", "Shared_genes_with_representative", "Jaccard_with_representative", "abs_NES", "Display_rank", "Sign", "Cluster", "Term_number", "Display_cluster"], "kinds": ["string", "string", "string", "string", "string", "number", "number", "number", "number", "number", "string", "boolean", "string", "string", "number", "string", "string", "number", "number", "number", "number", "number", "string", "string", "number", "string"], "target": "EnrichmentMap_AutoAnnotate/results/displayed_terms_with_clusters.csv", "rows": 40}, {"name": "试排_功能簇统计", "columns": ["Analysis", "Cluster", "n_terms", "positive", "negative", "anchor_GO", "anchor_label", "min_p", "min_q", "displayed"], "kinds": ["string", "string", "number", "number", "number", "string", "string", "number", "number", "boolean"], "target": "Functional_cluster_preview/results/all_cluster_membership_summary.csv", "rows": 386}, {"name": "试排_完整边表", "columns": ["Analysis", "source", "target", "shared_genes", "Jaccard", "Overlap", "Combined"], "kinds": ["string", "string", "string", "number", "number", "number", "number"], "target": "Functional_cluster_preview/results/all_overlap_edges.csv", "rows": 5566}, {"name": "试排_分析汇总", "columns": ["Analysis", "significant_terms", "positive", "negative", "edges", "clusters", "displayed_clusters", "displayed_terms", "displayed_fraction", "converged"], "kinds": ["string", "number", "number", "number", "number", "number", "number", "number", "number", "boolean"], "target": "Functional_cluster_preview/results/analysis_summary.csv", "rows": 4}, {"name": "试排_绘图代表条目", "columns": ["Analysis", "Variant", "Cluster", "GO", "Description", "NES", "pval", "padj", "Direction", "size", "abs_NES"], "kinds": ["string", "string", "string", "string", "string", "number", "number", "number", "string", "number", "number"], "target": "Functional_cluster_preview/results/displayed_representatives_P_and_NES.csv", "rows": 80}, {"name": "试排_全部候选条目", "columns": ["Analysis", "Variant", "Cluster", "GO", "Description", "NES", "pval", "padj", "Direction", "size"], "kinds": ["string", "string", "string", "string", "string", "number", "number", "number", "string", "number"], "target": "Functional_cluster_preview/results/every_cluster_direction_candidate_P_and_NES.csv", "rows": 804}, {"name": "试排_固定坐标", "columns": ["Analysis", "GO", "Cluster", "x", "y"], "kinds": ["string", "string", "string", "number", "number"], "target": "Functional_cluster_preview/results/fixed_network_coordinates.csv", "rows": 1563}, {"name": "试排_全局前五对照", "columns": ["GO", "pval", "padj", "log2err", "ES", "NES", "size", "BH_adjusted_P", "Analysis", "Tissue", "Sample_subset", "Namespace", "Description", "Significant", "Direction", "Leading_edge_n", "Leading_edge_positive_coefficient_fraction", "Leading_edge_median_Pearson_r", "Set_positive_coefficient_fraction", "Set_median_Pearson_r", "abs_NES", "Variant", "Rank"], "kinds": ["string", "number", "number", "number", "number", "number", "number", "number", "string", "string", "string", "string", "string", "boolean", "string", "number", "number", "number", "number", "number", "number", "string", "number"], "target": "Functional_cluster_preview/results/global_top5_each_direction_comparison.csv", "rows": 80}, {"name": "试排_P与NES选择对照", "columns": ["Analysis", "P_terms", "NES_terms", "Shared_terms", "P_only", "NES_only"], "kinds": ["string", "number", "number", "number", "string", "string"], "target": "Functional_cluster_preview/results/representative_selection_comparison.csv", "rows": 4}])----",simplifyVector=FALSE)
  archive_specs <- jsonlite::fromJSON(r"----([{"target": "EnrichmentMap_AutoAnnotate/Summary_network_example/cytoscape/summary_command.json", "sha256": "7f360bedbde69076e590689d40023bf40284f3db07a0e0f7879146ad724bc0eb", "parts": 1}, {"target": "EnrichmentMap_AutoAnnotate/Summary_network_example/cytoscape/summary_network.json", "sha256": "77159015902c5c49d509aaf7d4c562ccdac24d227eb689ed0a995e3b06a5b9d0", "parts": 13}, {"target": "EnrichmentMap_AutoAnnotate/Summary_network_example/data/source_parameters.json", "sha256": "8b0b702df096f46f2cea855e54f272fc7b3e987258e54e384d20c01832105229", "parts": 1}, {"target": "EnrichmentMap_AutoAnnotate/Summary_network_example/parameters.json", "sha256": "47c45d65b5f70701c40a85efa42155f9c2b68da045555c23aff9913b21b5dab8", "parts": 1}, {"target": "EnrichmentMap_AutoAnnotate/cytoscape/GO_biomass_EnrichmentMap.cys", "sha256": "b639f674472b736482fa434b184a56bbaca7b21c43210cf3805d3ee890ecaca5", "parts": 367}, {"target": "EnrichmentMap_AutoAnnotate/cytoscape/Root__PE0.5_vs_CK/autoannotate.json", "sha256": "8e3fcc5019ad2dca7925e94d15e423e699adf612eed31530d8faf9ff8a6f2872", "parts": 1}, {"target": "EnrichmentMap_AutoAnnotate/cytoscape/Root__PE0.5_vs_CK/enrichment.tsv", "sha256": "34c278e7b860483225d812d91095fff6ab3480e0217db67d2ede8dd0ef0d4595", "parts": 2}, {"target": "EnrichmentMap_AutoAnnotate/cytoscape/Root__PE0.5_vs_CK/genesets.gmt", "sha256": "ca1078e3f40797a6897dbd1d3a09d5947bd62549d3c0fa016540f4d4bb5587e1", "parts": 15}, {"target": "EnrichmentMap_AutoAnnotate/cytoscape/Root__PE0.5_vs_CK/network.json", "sha256": "571f378ff25a237274591f02e66d1cab5b3a55dbb5f03be1e9299c33e7123f05", "parts": 34}, {"target": "EnrichmentMap_AutoAnnotate/cytoscape/Root__PVC5_vs_CK/autoannotate.json", "sha256": "2818bcd09de3cfb7ae567699e6271e6c6785373795488bad76621a5b65691c85", "parts": 1}, {"target": "EnrichmentMap_AutoAnnotate/cytoscape/Root__PVC5_vs_CK/enrichment.tsv", "sha256": "d96260c29e5a0eecfc04cb3ab676a30d936dfb0f51c5944012be96b91454ec50", "parts": 2}, {"target": "EnrichmentMap_AutoAnnotate/cytoscape/Root__PVC5_vs_CK/genesets.gmt", "sha256": "6f42b3a1cdc9bef098f39c2dff13585bbf98a3af333da18c155f2278cd4956c2", "parts": 26}, {"target": "EnrichmentMap_AutoAnnotate/cytoscape/Root__PVC5_vs_CK/network.json", "sha256": "6e2342b1af8e791434a191202251ba25ba9774fe6334b05c7181e9a3c495fc2c", "parts": 71}, {"target": "EnrichmentMap_AutoAnnotate/cytoscape/Shoot__PE0.5_vs_CK/autoannotate.json", "sha256": "5bc419944a2a4f3f1bdc5525d1f7a81b0a1345cb2a5355a3ae8dbd8ff2a90400", "parts": 1}, {"target": "EnrichmentMap_AutoAnnotate/cytoscape/Shoot__PE0.5_vs_CK/enrichment.tsv", "sha256": "5abe340623d0c7a850c3073f10f25c9effb30059fba0e6ba4fab5901e64a369c", "parts": 2}, {"target": "EnrichmentMap_AutoAnnotate/cytoscape/Shoot__PE0.5_vs_CK/genesets.gmt", "sha256": "86971ca217e076877089a2fdc469e52e75e6b7d0bce47f1784713c42b411930b", "parts": 17}, {"target": "EnrichmentMap_AutoAnnotate/cytoscape/Shoot__PE0.5_vs_CK/network.json", "sha256": "be7cfa7f83da41e333bd6266eab22cbc8cfb3dc80ff816f33b92aeae1fba68bd", "parts": 52}, {"target": "EnrichmentMap_AutoAnnotate/cytoscape/Shoot__PVC5_vs_CK/autoannotate.json", "sha256": "e6c5ad9d7561754e1690477895d0fda1986ace7e2884a795b5074b87946d26ac", "parts": 1}, {"target": "EnrichmentMap_AutoAnnotate/cytoscape/Shoot__PVC5_vs_CK/enrichment.tsv", "sha256": "2ee80d9ec1bda219084a7ca9255aca94523d86bc1d1c4148c9e9a8b5d8e7854e", "parts": 1}, {"target": "EnrichmentMap_AutoAnnotate/cytoscape/Shoot__PVC5_vs_CK/genesets.gmt", "sha256": "6c3aa08983abf9539153b6e90fb060a7e0b986406279ab0d7e472018d4f126fd", "parts": 9}, {"target": "EnrichmentMap_AutoAnnotate/cytoscape/Shoot__PVC5_vs_CK/network.json", "sha256": "be79dfb990e8e187c2a9ac3697c0ca048afb122909c33a76af2a2fdd004f08e1", "parts": 19}, {"target": "EnrichmentMap_AutoAnnotate/cytoscape/network_ids.json", "sha256": "f31d420ffc6af45403bf0e29138f8ba21d246c7af60f45676eac3ca94d19c5b6", "parts": 1}, {"target": "EnrichmentMap_AutoAnnotate/data/matched_gene_sets.json", "sha256": "fa124862b9958b9c854a08240c0fcaa4269804c2dfb45d0c2e5b498401ed375e", "parts": 63}, {"target": "EnrichmentMap_AutoAnnotate/data/original_GSEA_methods.json", "sha256": "3673fcaa11b51f254cdca842a43f640b10b5b8fa389e6053bc57efde840b24c3", "parts": 1}, {"target": "EnrichmentMap_AutoAnnotate/parameters.json", "sha256": "8b0b702df096f46f2cea855e54f272fc7b3e987258e54e384d20c01832105229", "parts": 1}, {"target": "EnrichmentMap_AutoAnnotate/parameters_P_variant.json", "sha256": "d5dabbc4977c9abf44a83cf4da7d70bedc212854b640af8537f07644cf276860", "parts": 1}, {"target": "Functional_cluster_preview/data/matched_gene_sets.json", "sha256": "fa124862b9958b9c854a08240c0fcaa4269804c2dfb45d0c2e5b498401ed375e", "parts": 63}, {"target": "Functional_cluster_preview/data/original_GSEA_methods.json", "sha256": "3673fcaa11b51f254cdca842a43f640b10b5b8fa389e6053bc57efde840b24c3", "parts": 1}, {"target": "Functional_cluster_preview/parameters.json", "sha256": "766678354f9379e83df5b0ce7eb4fa071128206852cbf88d8a1652713a1c688f", "parts": 1}])----",simplifyVector=FALSE)
  cache <- list()
  for (spec in specs) {
    if (is.null(cache[[spec$name]])) {
      types <- vapply(spec$kinds,function(k) switch(k,number='numeric',boolean='logical',string='text'),character(1))
      d <- as.data.frame(readxl::read_excel(input,sheet=spec$name,col_types=types,.name_repair='minimal',na=character()),check.names=FALSE)
      if (!identical(names(d),unlist(spec$columns)) || nrow(d)!=spec$rows) stop('Unexpected data layout: ',spec$name)
      for (j in seq_along(types)) if(types[j]=='text') d[[j]][is.na(d[[j]])] <- ''
      cache[[spec$name]] <- d
    }
    destination <- file.path(stage,'data',spec$target)
    dir.create(dirname(destination),recursive=TRUE,showWarnings=FALSE)
    write.csv(cache[[spec$name]],destination,row.names=FALSE,na='',fileEncoding='UTF-8')
  }
  storage <- as.data.frame(readxl::read_excel(input,sheet='网络工程与输入存档',col_types=c('text','numeric','text')))
  for(spec in archive_specs) {
    blocks <- storage[storage$Source==spec$target,,drop=FALSE]
    blocks <- blocks[order(blocks$Part),,drop=FALSE]
    stopifnot(nrow(blocks)==spec$parts,identical(as.integer(blocks$Part),seq_len(spec$parts)))
    content <- memDecompress(base64enc::base64decode(paste0(blocks$Gzip_Base64,collapse='')),type='gzip')
    if (digest::digest(content,algo='sha256',serialize=FALSE)!=spec$sha256) stop('Changed archive payload: ',spec$target)
    destination <- file.path(stage,'data',spec$target)
    dir.create(dirname(destination),recursive=TRUE,showWarnings=FALSE);writeBin(content,destination)
  }
  genes <- as.data.frame(readxl::read_excel(input,sheet='GO基因集',col_types=c('text','text','numeric','numeric','text')))
  gene_sets <- list()
  for(a in unique(genes$Analysis)) {
    group <- genes[genes$Analysis==a,,drop=FALSE];gene_sets[[a]] <- list()
    for(go in unique(group$GO)) {
      rows <- group[group$GO==go,,drop=FALSE];rows <- rows[order(rows$Part),,drop=FALSE]
      values <- unlist(strsplit(paste(rows$Genes,collapse=';'),';',fixed=TRUE),use.names=FALSE)
      if(length(values)!=rows$Gene_count[1] || anyDuplicated(values)) stop('Invalid gene set: ',a,' ',go)
      gene_sets[[a]][[go]] <- values
    }
  }
  for(branch in c('Functional_cluster_preview','EnrichmentMap_AutoAnnotate')) {
    jsonlite::write_json(gene_sets,file.path(stage,'data',branch,'data/matched_gene_sets.json'),auto_unbox=FALSE)
  }
  raw <- cache[['原始数据']]
  if(any(!is.finite(raw$NES)) || any(raw$padj>=0.05) || anyDuplicated(raw[c('Analysis','GO')])) stop('Invalid significant GO inputs.')
  for(a in unique(raw$Analysis)) {
    z<-raw[raw$Analysis==a,,drop=FALSE]
    counts<-vapply(z$GO,function(go)length(gene_sets[[a]][[go]]),integer(1))
    if(any(counts!=z$size))stop('Gene-set sizes no longer match the GSEA input for ',a)
  }
  stopifnot(identical(unname(tools::md5sum(input)),unname(original_hash)))
  message('Workbook read successfully: ',length(specs)+length(archive_specs),' source files; ',nrow(raw),' significant GO records; ',length(unique(raw$Analysis)),' analyses.')
  if ('--check-only' %in% args) {message('Input, original network archives and gene sets passed verification.');return(invisible(TRUE))}
  code_files <- list(
    r"----(EnrichmentMap_AutoAnnotate/Summary_network_example/build_summary.py)----" = r"----(from pathlib import Path as _Fig5Path
def _fig5_root():
    return next(p for p in _Fig5Path(__file__).resolve().parents if all((p / folder).is_dir() for folder in ('code', 'data', 'figure')))
def _fig5_analysis(relative):
    return _fig5_root() / 'data' / relative.removeprefix('GO_biomass_GSEA/')
def _fig5_figures(relative):
    p = _fig5_root() / 'figure' / relative
    p.mkdir(parents=True, exist_ok=True)
    return p
"""Run against the original, annotated Cytoscape session via CyREST."""
from pathlib import Path
import urllib.request,json
B=_fig5_analysis('GO_biomass_GSEA/EnrichmentMap_AutoAnnotate/Summary_network_example');URL='http://127.0.0.1:1234/v1'
def request(path,data=None):
 req=urllib.request.Request(URL+path,data=None if data is None else json.dumps(data).encode(),headers={'Content-Type':'application/json'})
 return json.load(urllib.request.urlopen(req,timeout=120))
# Open ../cytoscape/GO_biomass_EnrichmentMap.cys first. Match the exact original network name.
source=[]
for sid in request('/networks'):
 net=request('/networks/'+str(sid));name=net.get('data',{}).get('name','')
 if name=='Shoot__PE0.5_vs_CK':source.append(sid)
assert len(source)==1, 'Open original annotated session, with exactly one matching source network.'
args={'network':'SUID:'+str(source[0]),'includeUnclustered':'true'}
before=set(request('/networks'));response=request('/commands/autoannotate/summary',args);after=set(request('/networks'));new=after-before
assert not response.get('errors') and len(new)==1
sid=new.pop();net=request('/networks/'+str(sid));assert len(net['elements']['nodes'])==101
(B/'cytoscape/summary_network.json').write_text(json.dumps(net))
(B/'cytoscape/summary_command.json').write_text(json.dumps({'network':args['network'],'includeUnclustered':True,'response':response,'network_id':sid},indent=2))
print('Created full Summary Network:',sid)
)----",
    r"----(EnrichmentMap_AutoAnnotate/Summary_network_example/plot_example.py)----" = r"----(from pathlib import Path as _Fig5Path
def _fig5_root():
    return next(p for p in _Fig5Path(__file__).resolve().parents if all((p / folder).is_dir() for folder in ('code', 'data', 'figure')))
def _fig5_analysis(relative):
    return _fig5_root() / 'data' / relative.removeprefix('GO_biomass_GSEA/')
def _fig5_figures(relative):
    p = _fig5_root() / 'figure' / relative
    p.mkdir(parents=True, exist_ok=True)
    return p
from pathlib import Path
import os
os.environ.setdefault('MPLCONFIGDIR','/tmp/fig5_mpl_cache')
import json,math,textwrap,xml.etree.ElementTree as ET
import numpy as np,pandas as pd,networkx as nx
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from matplotlib.patches import Wedge,Circle
from matplotlib.collections import LineCollection
from matplotlib import font_manager
B=_fig5_analysis('GO_biomass_GSEA/EnrichmentMap_AutoAnnotate/Summary_network_example');F=_fig5_figures('EnrichmentMap_AutoAnnotate/Summary_network_example');R=B/'results'
C=pd.read_csv(R/'summary_nodes.csv');E=pd.read_csv(R/'summary_edges.csv');T=pd.read_csv(B/'data/displayed_terms_with_clusters.csv');N=pd.read_csv(B/'data/all_nodes.csv')
font_manager.findfont('Arial',fallback_to_default=False)
plt.rcParams.update({'font.family':'Arial','font.size':8,'pdf.fonttype':42,'svg.fonttype':'none','axes.linewidth':.6,'savefig.facecolor':'white'})
RED='#D6503A';BLUE='#286FA8';G=nx.Graph();G.add_nodes_from(C.Cluster)
for r in E.itertuples():G.add_edge(r.source,r.target,weight=r.GO_pair_count)
pos=nx.spring_layout(G,seed=20261028,k=.33,iterations=500,weight='weight',scale=1)
ids=C.Cluster.tolist();xy=np.array([pos[g] for g in ids]);radii=.023*np.sqrt(C.Terms.to_numpy())
# Collision removal only affects visual coordinates; graph and statistics are fixed.
for _ in range(500):
 maxmove=0.
 for i in range(len(ids)):
  for j in range(i+1,len(ids)):
   d=xy[j]-xy[i];dist=np.linalg.norm(d);target=radii[i]+radii[j]+.015
   if dist<target:
    u=d/max(dist,1e-12) if dist>1e-12 else np.array([1.,0.]);move=(target-dist)*.51
    xy[i]-=u*move;xy[j]+=u*move;maxmove=max(maxmove,move)
 if maxmove<1e-6:break
xy-=xy.mean(0);pos={g:p for g,p in zip(ids,xy)};maxR=max(np.max(abs(xy[:,0])+radii),np.max(abs(xy[:,1])+radii))+.15
pd.DataFrame([{'Cluster':g,'x':pos[g][0],'y':pos[g][1],'radius':radii[i]} for i,g in enumerate(ids)]).to_csv(R/'fixed_summary_coordinates.csv',index=False)
top=set(C.head(10).Cluster);selected=set(T.Cluster)
def graph(ax,which='all',labels=True):
 visible=set(ids) if which=='all' else selected
 ed=[r for r in E.itertuples() if r.source in visible and r.target in visible]
 ax.add_collection(LineCollection([[pos[r.source],pos[r.target]] for r in ed],colors='#aaaeb2',linewidths=[.4+.16*np.log1p(r.GO_pair_count) for r in ed],alpha=.6,zorder=0))
 for i,r in enumerate(C.itertuples()):
  if r.Cluster not in visible:continue
  centre=pos[r.Cluster];rad=radii[i];angle=360*r.Positive/r.Terms
  for j,(lo,hi,color) in enumerate([(90,90+angle,RED),(90+angle,450,BLUE)]):
   if hi-lo<1e-10:continue
   w=Wedge(centre,rad,lo,hi,facecolor=color,edgecolor='white',linewidth=.25,zorder=2)
   w.set_gid(f'cluster_{r.Cluster}_{j}');ax.add_patch(w)
  ax.add_patch(Circle(centre,rad,fill=False,edgecolor='#222222' if r.Cluster in selected else '#777777',linewidth=.95 if r.Cluster in selected else .3,zorder=3))
  if labels and (r.Cluster in top or r.Cluster in selected):
   ax.annotate(r.Cluster,(centre[0],centre[1]+rad),xytext=(0,2),textcoords='offset points',ha='center',va='bottom',fontsize=7.4,color='#222',zorder=5,bbox=dict(facecolor='white',edgecolor='none',pad=.25,alpha=.85))
 ax.set_xlim(-maxR,maxR);ax.set_ylim(-maxR,maxR);ax.set_aspect('equal');ax.axis('off')
def table(ax,z,positive):
 ax.set_xlim(0,1);ax.set_ylim(0,1);ax.axis('off');col=RED if positive else BLUE
 ax.text(0,.99,'Positive NES' if positive else 'Negative NES',color=col,weight='bold',va='top',fontsize=9)
 ax.text(.81,.99,'NES',ha='right',va='top',weight='bold');ax.text(1,.99,'q',ha='right',va='top',weight='bold')
 ax.plot([0,1],[.89,.89],color='#aaa',lw=.5)
 for j,r in enumerate(z.itertuples()):
  y=.80-j*.168
  ax.text(0,y,textwrap.fill(f'[{r.Cluster}] '+r.Description,48,break_long_words=False),color=col,va='center',fontsize=8,linespacing=1.05)
  ax.text(.81,y,f'{r.NES:+.2f}',ha='right',va='center',color=col,fontsize=8)
  ax.text(1,y,f'{r.Original_BH_P:.1e}' if r.Original_BH_P<.001 else f'{r.Original_BH_P:.3g}',ha='right',va='center',fontsize=7.5)
fig=plt.figure(figsize=(12,10.4));fig.text(.04,.97,'Stem and leaves | CK + PE0.5',fontsize=12,weight='bold',va='top')
fig.text(.04,.94,'Biomass-associated GSEA | n = 8 | 484 significant GO terms | 101 clusters',fontsize=9,va='top')
gs=fig.add_gridspec(2,2,left=.04,right=.965,top=.90,bottom=.075,height_ratios=[.93,2.0],width_ratios=[1,1],hspace=.23,wspace=.15)
for j,positive in enumerate([True,False]):
 ax=fig.add_subplot(gs[0,j]);z=T[T.NES*(1 if positive else -1)>0].sort_values('NES',ascending=not positive);table(ax,z,positive)
 if j==0:ax.text(-.04,1.04,'a',transform=ax.transAxes,fontsize=13,weight='bold')
bottom=gs[1,:].subgridspec(1,2,width_ratios=[1.58,1],wspace=.10)
ax=fig.add_subplot(bottom[0]);graph(ax);ax.text(-.01,1.03,'b',transform=ax.transAxes,fontsize=13,weight='bold');ax.text(.04,1.03,'All 101 clusters · 484 GO terms · 100% coverage',transform=ax.transAxes,fontsize=9,va='center')
ac=fig.add_subplot(bottom[1]);ac.axis('off');ac.set_xlim(0,1);ac.set_ylim(0,1)
ac.text(0,1.03,'c',fontsize=13,weight='bold',transform=ac.transAxes);ac.text(.07,1.03,'Largest clusters by GO-term count',fontsize=9,va='center')
for x,label in [(0,'Cluster'),(.76,'GO'),(.98,'+ / −')]:ac.text(x,.975,label,ha='right' if x else 'left',weight='bold',fontsize=8)
ac.plot([0,1],[.95,.95],color='#aaa',lw=.5)
for j,r in enumerate(C.head(10).itertuples()):
 y=.915-j*.073
 ac.text(0,y,r.Cluster,fontsize=8,va='top',weight='bold');ac.text(.14,y,textwrap.fill(r.Label,28,break_long_words=False),fontsize=8,va='top',linespacing=1.0)
 ac.text(.76,y,str(r.Terms),ha='right',va='top',fontsize=8);ac.text(.98,y,f'{r.Positive} / {r.Negative}',ha='right',va='top',fontsize=8)
ac.text(0,.15,'All significant GO terms',weight='bold',fontsize=8)
ac.text(0,.105,'Positive NES: 82 (16.9%)',color=RED,fontsize=8.5);ac.text(0,.06,'Negative NES: 402 (83.1%)',color=BLUE,fontsize=8.5)
fig.text(.04,.045,'Node area: GO-term count within a cluster. Sectors: positive / negative NES proportions.\nBlack outline: cluster containing an upper representative term. Edges: connections between original clusters.',fontsize=8,va='center')
for ext in ['svg','tiff']:
 kw={'dpi':400}
 if ext=='tiff':kw['pil_kwargs']={'compression':'tiff_lzw'}
 fig.savefig(F/f'Shoot_PE_full_summary_example.{ext}',**kw)
plt.close(fig)
fig,axs=plt.subplots(1,2,figsize=(12,6.4));fig.subplots_adjust(left=.025,right=.975,top=.86,bottom=.12,wspace=.06)
for ax,which,title in [(axs[0],'selected','Current display: 8 clusters · 57 GO terms (11.8%)'),(axs[1],'all','Full summary: 101 clusters · 484 GO terms (100%)')]:
 graph(ax,which);ax.set_title(title,fontsize=10,pad=16)
fig.text(.04,.96,'Stem and leaves | CK + PE0.5',fontsize=12,weight='bold',va='top');fig.text(.04,.035,'Identical coordinates, node-size scale and colours in both panels. Each node represents one complete cluster.',fontsize=8)
for ext in ['svg','tiff']:
 kw={'dpi':400}
 if ext=='tiff':kw['pil_kwargs']={'compression':'tiff_lzw'}
 fig.savefig(F/f'Local_vs_full_coverage.{ext}',**kw)
plt.close(fig)
fig,ax=plt.subplots(figsize=(9,9));fig.subplots_adjust(left=.02,right=.98,top=.98,bottom=.02);graph(ax)
fig.savefig(F/'all_clusters_interactive.svg');plt.close(fig)
# Add complete membership tooltips to the SVG; all 101 clusters remain present.
ns='http://www.w3.org/2000/svg';ET.register_namespace('',ns);p=F/'all_clusters_interactive.svg';root=ET.fromstring(p.read_text())
for g in root.iter():
 gid=g.get('id','')
 if gid.startswith('cluster_C'):
  cid=gid.split('_')[1];r=C[C.Cluster==cid].iloc[0];z=N[N.Cluster==cid]
  tip=f'{cid} | {r.Label}\n{r.Terms} GO terms: +NES {r.Positive}; -NES {r.Negative}\n'+'\n'.join(f'{v.GO} {v.Description} | NES {v.NES:+.2f}; q {v.padj:.3g}' for v in z.itertuples())
  title=ET.Element('{'+ns+'}title');title.text=tip;g.insert(0,title);g.set('class','cluster');g.set('data-cluster',cid)
p.write_text(ET.tostring(root,encoding='unicode'))
print('Rendered example, coverage comparison and interactive all-cluster SVG.')
)----",
    r"----(EnrichmentMap_AutoAnnotate/Summary_network_example/prepare_example.py)----" = r"----(from pathlib import Path as _Fig5Path
def _fig5_root():
    return next(p for p in _Fig5Path(__file__).resolve().parents if all((p / folder).is_dir() for folder in ('code', 'data', 'figure')))
def _fig5_analysis(relative):
    return _fig5_root() / 'data' / relative.removeprefix('GO_biomass_GSEA/')
def _fig5_figures(relative):
    p = _fig5_root() / 'figure' / relative
    p.mkdir(parents=True, exist_ok=True)
    return p
from pathlib import Path
import json,shutil,hashlib
import pandas as pd
B=_fig5_analysis('GO_biomass_GSEA/EnrichmentMap_AutoAnnotate/Summary_network_example');SRC=_fig5_analysis('GO_biomass_GSEA/EnrichmentMap_AutoAnnotate');A='Shoot__PE0.5_vs_CK'
if not (B/'data/all_nodes.csv').exists():
 for name in ['all_nodes.csv','all_edges.csv','clusters.csv','displayed_terms_with_clusters.csv']:
  d=pd.read_csv(SRC/'results'/name);d[d.Analysis==A].to_csv(B/'data'/name,index=False)
 shutil.copy2(SRC/'parameters.json',B/'data/source_parameters.json')
N=pd.read_csv(B/'data/all_nodes.csv');E=pd.read_csv(B/'data/all_edges.csv');C=pd.read_csv(B/'data/clusters.csv');T=pd.read_csv(B/'data/displayed_terms_with_clusters.csv')
net=json.loads((B/'cytoscape/summary_network.json').read_text());lookup={frozenset(str(r.GO_members).split(';')):r.Cluster for r in C.itertuples()};nativeid={}
for row in net['elements']['nodes']:
 d=row['data'];members=frozenset(d['EnrichmentMap_Name'].split(','));assert members in lookup
 nativeid[d['id']]=lookup[members]
assert len(nativeid)==len(C)==101 and len(set(nativeid.values()))==101
native_edges={tuple(sorted((nativeid[x['data']['source']],nativeid[x['data']['target']]))) for x in net['elements']['edges']}
nodecluster=N.set_index('GO').Cluster.to_dict();rows=[]
for r in E.itertuples():
 u,v=nodecluster[r.source],nodecluster[r.target]
 if u!=v:
  u,v=sorted((u,v));rows.append({'source':u,'target':v,'GO_pair_count':1})
SE=pd.DataFrame(rows).groupby(['source','target'],as_index=False).GO_pair_count.sum()
assert set(zip(SE.source,SE.target))==native_edges and len(SE)==53
SE.to_csv(B/'results/summary_edges.csv',index=False)
C['Selected_by_upper_table']=C.Cluster.isin(T.Cluster);C['Positive_fraction']=C.Positive/C.Terms
C.to_csv(B/'results/summary_nodes.csv',index=False)
assert C.Terms.sum()==len(N)==484 and C.Positive.sum()==82 and C.Negative.sum()==402
assert C.loc[C.Selected_by_upper_table,'Terms'].sum()==57
pd.DataFrame([{'Display':'Current representative clusters','Clusters':8,'GO_terms':57,'GO_coverage':57/484},{'Display':'Full summary network','Clusters':101,'GO_terms':484,'GO_coverage':1}]).to_csv(B/'results/coverage_comparison.csv',index=False)
validation={'Status':'PASS','Analysis':A,'Original_significant_GO':484,'Original_positive_GO':82,'Original_negative_GO':402,'Native_summary_nodes':101,'Native_summary_edges':53,'All_memberships_match_original_clusters':True,'All_summary_edges_match_original_between_cluster_edges':True,'GO_coverage':1.0,'GSEA_or_clustering_rerun':False,'Layout_only_label_rule':'Label ten largest clusters by GO count plus upper-table cluster identifiers; all 101 clusters remain in the graph.'}
print(validation)
)----",
    r"----(EnrichmentMap_AutoAnnotate/compare_rankings.py)----" = r"----(from pathlib import Path as _Fig5Path
def _fig5_root():
    return next(p for p in _Fig5Path(__file__).resolve().parents if all((p / folder).is_dir() for folder in ('code', 'data', 'figure')))
def _fig5_analysis(relative):
    return _fig5_root() / 'data' / relative.removeprefix('GO_biomass_GSEA/')
def _fig5_figures(relative):
    p = _fig5_root() / 'figure' / relative
    p.mkdir(parents=True, exist_ok=True)
    return p
from pathlib import Path
import pandas as pd,json,hashlib,html
from PIL import Image
B=_fig5_analysis('GO_biomass_GSEA/EnrichmentMap_AutoAnnotate');R=B/'results';F=_fig5_figures('EnrichmentMap_AutoAnnotate')
n=pd.read_csv(R/'displayed_terms_with_clusters.csv');p=pd.read_csv(R/'displayed_terms_P_with_clusters.csv');ret=pd.read_csv(B/'data/GO_BP_retained_representatives.csv');N=pd.read_csv(R/'all_nodes.csv');C=pd.read_csv(R/'clusters.csv');rows=[]
for a,z in p.groupby('Analysis',sort=False):
 old=n[n.Analysis==a]
 for sign in [1,-1]:
  zz=ret[(ret.Analysis==a)&(ret.NES*sign>0)&(ret.Original_BH_P<.05)].copy();zz['abs']=zz.NES.abs()
  expected=zz.sort_values(['Original_P','abs','Original_BH_P','GO'],ascending=[True,False,True,True]).head(5)
  assert set(expected.GO)==set(z[z.NES*sign>0].GO)
 assert len(z)==10 and (z.Original_BH_P<.05).all()
 rows.append({'Analysis':a,'Shared_terms':len(set(z.GO)&set(old.GO)),'NES_only':'; '.join(old[~old.GO.isin(z.GO)].Description),'P_only':'; '.join(z[~z.GO.isin(old.GO)].Description)})
comparison=pd.DataFrame(rows);comparison.to_csv(R/'P_vs_NES_selection_comparison.csv',index=False)
sn=pd.read_csv(R/'display_summary.csv');sp=pd.read_csv(R/'display_summary_P.csv');scope=sn[['Analysis','Significant_terms','All_clusters','Displayed_clusters','Terms_in_displayed_clusters']].rename(columns={'Displayed_clusters':'NES_displayed_clusters','Terms_in_displayed_clusters':'NES_displayed_GO'}).merge(sp[['Analysis','Displayed_clusters','Terms_in_displayed_clusters']].rename(columns={'Displayed_clusters':'P_displayed_clusters','Terms_in_displayed_clusters':'P_displayed_GO'}),on='Analysis');scope.to_csv(R/'P_vs_NES_display_scope.csv',index=False)
params={'P_variant_selection':'From existing collapsePathways retained q<0.05 terms; positive/negative NES separately, raw P ascending; ties |NES| descending, q ascending, GO ascending; 5 per sign.','NES_selection':'Same retained candidate set; |NES| descending, split by sign; 5 per sign.','Lower_clustering':'Same original EnrichmentMap/AutoAnnotate clusters. No GSEA/network/clustering rerun.','Lower_display':'Union of complete clusters containing upper selected terms; not the 10 largest clusters, not a display of all significant GO terms.','Shared_display_records':int(comparison.Shared_terms.sum())}
(B/'parameters_P_variant.json').write_text(json.dumps(params,ensure_ascii=False,indent=2))
for ext in ['svg','tiff']:assert (F/f'GO_biomass_functional_clusters_rank_P.{ext}').stat().st_size>1000
with Image.open(F/'GO_biomass_functional_clusters_rank_P.tiff') as im:assert im.size==(4720,5840)
print(scope.to_string(index=False));print('P variant PASS; shared selected records:',int(comparison.Shared_terms.sum()))
)----",
    r"----(EnrichmentMap_AutoAnnotate/cyrest.py)----" = r"----(from pathlib import Path as _Fig5Path
def _fig5_root():
    return next(p for p in _Fig5Path(__file__).resolve().parents if all((p / folder).is_dir() for folder in ('code', 'data', 'figure')))
def _fig5_analysis(relative):
    return _fig5_root() / 'data' / relative.removeprefix('GO_biomass_GSEA/')
def _fig5_figures(relative):
    p = _fig5_root() / 'figure' / relative
    p.mkdir(parents=True, exist_ok=True)
    return p
import json,urllib.request,urllib.parse
from pathlib import Path
BASE='http://127.0.0.1:1234/v1'
ROOT=_fig5_analysis('GO_biomass_GSEA/EnrichmentMap_AutoAnnotate')
def request(path,data=None,method=None):
 b=None if data is None else json.dumps(data).encode()
 req=urllib.request.Request(BASE+path,data=b,headers={'Content-Type':'application/json'},method=method)
 with urllib.request.urlopen(req,timeout=300) as f:
  s=f.read().decode()
  try:return json.loads(s)
  except:return s

def cmd(namespace,name,params=None):
 r=request('/commands/'+urllib.parse.quote(namespace)+'/'+urllib.parse.quote(name),params or {},'POST')
 with (ROOT/'cytoscape/commands.jsonl').open('a') as f:f.write(json.dumps({'namespace':namespace,'command':name,'params':params,'response':r})+'\n')
 if r.get('errors'):raise RuntimeError(r)
 return r
if __name__=='__main__':
 print(request('/version'))
 for name in ['enrichmentmap','autoannotate','cluster']:
  v=request('/commands/'+name);print(name,str(v)[:3000]);(ROOT/'cytoscape'/f'{name}_commands.json').write_text(json.dumps(v,indent=2))
)----",
    r"----(EnrichmentMap_AutoAnnotate/plot_P_variant.py)----" = r"----(from pathlib import Path as _Fig5Path
def _fig5_root():
    return next(p for p in _Fig5Path(__file__).resolve().parents if all((p / folder).is_dir() for folder in ('code', 'data', 'figure')))
def _fig5_analysis(relative):
    return _fig5_root() / 'data' / relative.removeprefix('GO_biomass_GSEA/')
def _fig5_figures(relative):
    p = _fig5_root() / 'figure' / relative
    p.mkdir(parents=True, exist_ok=True)
    return p
from pathlib import Path
import os
os.environ.setdefault('MPLCONFIGDIR','/tmp/fig5_mpl_cache')
import json,math,textwrap
import numpy as np,pandas as pd,networkx as nx
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from matplotlib import colors,font_manager
from matplotlib.collections import LineCollection
from matplotlib.patches import Ellipse
B=_fig5_analysis('GO_biomass_GSEA/EnrichmentMap_AutoAnnotate');F=_fig5_figures('EnrichmentMap_AutoAnnotate');R=B/'results'
font=font_manager.findfont('Arial',fallback_to_default=False)
plt.rcParams.update({'font.family':'Arial','font.size':8,'axes.labelsize':8,'axes.titlesize':9,'axes.linewidth':.6,'pdf.fonttype':42,'svg.fonttype':'none','savefig.facecolor':'white'})
N=pd.read_csv(R/'all_nodes.csv');E=pd.read_csv(R/'all_edges.csv');T=pd.read_csv(R/'displayed_terms_P.csv');C=pd.read_csv(R/'clusters.csv')
order=T.Analysis.drop_duplicates().tolist();red='#D6503A';blue='#286FA8';cmap=colors.LinearSegmentedColormap.from_list('NES',[blue,'#f8f8f8',red]);norm=colors.TwoSlopeNorm(0,-3.5,3.5)
summary=[];coords=[];layoutchecks=[]

def cluster_network(ax,a,cluster,display_letter):
 n=N[(N.Analysis==a)&(N.Cluster==cluster)].copy();ids=n.GO.tolist();idset=set(ids)
 e=E[(E.Analysis==a)&E.source.isin(idset)&E.target.isin(idset)]
 G=nx.Graph();G.add_nodes_from(ids)
 for r in e.itertuples():G.add_edge(r.source,r.target,weight=r.Combined)
 pos=nx.spring_layout(G,seed=20261028,iterations=200,weight='weight') if len(ids)>1 else {ids[0]:np.zeros(2)}
 xy=np.array([pos[g] for g in ids]);xy-=xy.mean(0);xy/=max(abs(xy).max(),1e-9)
 pos={g:v for g,v in zip(ids,xy)}
 for g,v in pos.items():coords.append({'Analysis':a,'GO':g,'Cluster':cluster,'x':v[0],'y':v[1]})
 ax.set_aspect('equal');ax.set_xlim(-1.25,1.25);ax.set_ylim(-1.18,1.22);ax.axis('off')
 if len(ids)>1:
  ax.add_patch(Ellipse((0,0),2.43,2.22,facecolor='#f8f8f8',edgecolor='#c7c7c7',lw=.55,zorder=0))
  ax.add_collection(LineCollection([[pos[r.source],pos[r.target]] for r in e.itertuples()],colors='#8a8a8a',linewidths=.5,alpha=.55,zorder=1))
 sizes=12+62*np.sqrt(n['size'].to_numpy()/500)
 ax.scatter(xy[:,0],xy[:,1],s=sizes,c=n.NES,cmap=cmap,norm=norm,edgecolor='white',linewidth=.35,zorder=3)
 selected=T[(T.Analysis==a)&T.GO.isin(ids)]
 for r in selected.itertuples():
  k=ids.index(r.GO);x,y=pos[r.GO]
  ax.scatter([x],[y],s=sizes[k]+24,facecolors='none',edgecolors='#202020',linewidth=.9,zorder=4)
  ax.annotate(str(int(r.Term_number)),(x,y),xytext=(3,3),textcoords='offset points',fontsize=6.5,weight='bold',bbox=dict(facecolor='white',edgecolor='none',pad=.3,alpha=.95),zorder=5)
 cr=C[(C.Analysis==a)&(C.Cluster==cluster)].iloc[0]
 label=textwrap.fill(cr.Label,25,break_long_words=False)
 ax.set_title(f'{display_letter}  {label}',fontsize=7.2,pad=5,linespacing=1.05)
 ax.text(.5,-.025,f'{len(ids)} GO terms',transform=ax.transAxes,ha='center',fontsize=6.5,color='#555555')

fig=plt.figure(figsize=(11.8,14.6))
outer=fig.add_gridspec(2,2,left=.04,right=.97,top=.963,bottom=.065,wspace=.16,hspace=.18)
for i,a in enumerate(order):
 n=N[N.Analysis==a];c=C[C.Analysis==a];t=T[T.Analysis==a].copy()
 t['abs']=t.NES.abs();t=pd.concat([t[t.NES>0].sort_values(['Original_P','abs','Original_BH_P','GO'],ascending=[True,False,True,True]),t[t.NES<0].sort_values(['Original_P','abs','Original_BH_P','GO'],ascending=[True,False,True,True])])
 t['Term_number']=range(1,len(t)+1)
 T.loc[t.index,'Term_number']=t.Term_number
 chosen=t.Cluster.drop_duplicates().tolist();letters={cid:chr(65+j) for j,cid in enumerate(chosen)}
 T.loc[t.index,'Display_cluster']=t.Cluster.map(letters)
 sub=outer[i//2,i%2].subgridspec(2,1,height_ratios=[1.02,1.08],hspace=.12)
 at=fig.add_subplot(sub[0]);at.set_xlim(0,1);at.set_ylim(0,1);at.axis('off')
 tissue='Stem and leaves' if a.startswith('Shoot') else 'Roots';pair='CK + PE0.5' if 'PE0.5' in a else 'CK + PVC5'
 at.text(-.025,1.13,chr(97+i),fontsize=13,weight='bold',transform=at.transAxes,va='top')
 at.text(.035,1.13,f'{tissue} | {pair}',fontsize=10,weight='bold',va='top',transform=at.transAxes)
 at.text(.035,1.055,f'n = 8   |   {len(n)} significant GO terms   |   {len(c)} clusters',fontsize=7.8,transform=at.transAxes,va='top')
 at.text(0,.975,'GO biological process',fontsize=8,weight='bold',va='top');at.text(.72,.975,'NES',ha='right',fontsize=8,weight='bold',va='top');at.text(.86,.975,'P',ha='right',fontsize=8,weight='bold',va='top');at.text(1,.975,'q',ha='right',fontsize=8,weight='bold',va='top')
 at.plot([0,1],[.93,.93],color='#999999',lw=.5)
 for j,r in enumerate(t.itertuples()):
  y=.877-j*.084;col=red if r.NES>0 else blue
  label=f'{j+1} [{letters[r.Cluster]}]  '+r.Description
  at.text(.005,y,textwrap.fill(label,35,break_long_words=False,break_on_hyphens=False),fontsize=7.7,va='center',color=col,linespacing=1.0)
  at.text(.72,y,f'{r.NES:+.2f}',ha='right',va='center',fontsize=7.7,color=col)
  pv=r.Original_P;ps=f'{pv:.1e}' if pv<.001 else f'{pv:.3g}';at.text(.86,y,ps,ha='right',va='center',fontsize=7.3)
  q=r.Original_BH_P;qs=f'{q:.1e}' if q<.001 else f'{q:.3f}'
  at.text(1,y,qs,ha='right',va='center',fontsize=7.3)
 # These are complete clusters containing the displayed representatives, not a reclustered subset.
 cols=3 if len(chosen)>4 else 2;rows=math.ceil(len(chosen)/cols)
 ng=sub[1].subgridspec(rows,cols,wspace=.14,hspace=.98)
 for j,cid in enumerate(chosen):
  ax=fig.add_subplot(ng[j//cols,j%cols]);cluster_network(ax,a,cid,letters[cid])
 shown=N[(N.Analysis==a)&N.Cluster.isin(chosen)]
 summary.append(dict(Analysis=a,Significant_terms=len(n),Retained_terms=int((pd.read_csv(B/'data/GO_BP_retained_representatives.csv').Analysis==a).sum()),Displayed_terms=len(t),All_clusters=len(c),Displayed_clusters=len(chosen),Terms_in_displayed_clusters=len(shown),Displayed_cluster_GO_coverage=len(shown)/len(n)))
# One shared colour key for the same biomass-associated NES.
cax=fig.add_axes([.765,.026,.19,.009]);cb=fig.colorbar(plt.cm.ScalarMappable(norm=norm,cmap=cmap),cax=cax,orientation='horizontal',ticks=[-3,0,3]);cb.outline.set_visible(False);cb.ax.tick_params(labelsize=7,length=2);cb.set_label('Biomass-associated NES',fontsize=8,labelpad=3)
fig.text(.04,.035,'Node: GO term; size: gene-set size. Edge: shared genes.\nOutlined numbered nodes: terms listed above. Cluster labels: AutoAnnotate / WordCloud.',fontsize=7.3,va='center')
for ext in ['svg','tiff']:
 kw={'dpi':400}
 if ext=='tiff':kw['pil_kwargs']={'compression':'tiff_lzw'}
 fig.savefig(F/f'GO_biomass_functional_clusters_rank_P.{ext}',**kw)
plt.close(fig)
T.to_csv(R/'displayed_terms_P_with_clusters.csv',index=False);pd.DataFrame(summary).to_csv(R/'display_summary_P.csv',index=False);pd.DataFrame(coords).to_csv(R/'display_coordinates_P.csv',index=False)
print(pd.DataFrame(summary).to_string(index=False))
)----",
    r"----(EnrichmentMap_AutoAnnotate/plot_figure.py)----" = r"----(from pathlib import Path as _Fig5Path
def _fig5_root():
    return next(p for p in _Fig5Path(__file__).resolve().parents if all((p / folder).is_dir() for folder in ('code', 'data', 'figure')))
def _fig5_analysis(relative):
    return _fig5_root() / 'data' / relative.removeprefix('GO_biomass_GSEA/')
def _fig5_figures(relative):
    p = _fig5_root() / 'figure' / relative
    p.mkdir(parents=True, exist_ok=True)
    return p
from pathlib import Path
import os
os.environ.setdefault('MPLCONFIGDIR','/tmp/fig5_mpl_cache')
import json,math,textwrap
import numpy as np,pandas as pd,networkx as nx
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from matplotlib import colors,font_manager
from matplotlib.collections import LineCollection
from matplotlib.patches import Ellipse
B=_fig5_analysis('GO_biomass_GSEA/EnrichmentMap_AutoAnnotate');F=_fig5_figures('EnrichmentMap_AutoAnnotate');R=B/'results'
font=font_manager.findfont('Arial',fallback_to_default=False)
plt.rcParams.update({'font.family':'Arial','font.size':8,'axes.labelsize':8,'axes.titlesize':9,'axes.linewidth':.6,'pdf.fonttype':42,'svg.fonttype':'none','savefig.facecolor':'white'})
N=pd.read_csv(R/'all_nodes.csv');E=pd.read_csv(R/'all_edges.csv');T=pd.read_csv(R/'displayed_terms.csv');C=pd.read_csv(R/'clusters.csv')
order=T.Analysis.drop_duplicates().tolist();red='#D6503A';blue='#286FA8';cmap=colors.LinearSegmentedColormap.from_list('NES',[blue,'#f8f8f8',red]);norm=colors.TwoSlopeNorm(0,-3.5,3.5)
summary=[];coords=[];layoutchecks=[]

def cluster_network(ax,a,cluster,display_letter):
 n=N[(N.Analysis==a)&(N.Cluster==cluster)].copy();ids=n.GO.tolist();idset=set(ids)
 e=E[(E.Analysis==a)&E.source.isin(idset)&E.target.isin(idset)]
 G=nx.Graph();G.add_nodes_from(ids)
 for r in e.itertuples():G.add_edge(r.source,r.target,weight=r.Combined)
 pos=nx.spring_layout(G,seed=20261028,iterations=200,weight='weight') if len(ids)>1 else {ids[0]:np.zeros(2)}
 xy=np.array([pos[g] for g in ids]);xy-=xy.mean(0);xy/=max(abs(xy).max(),1e-9)
 pos={g:v for g,v in zip(ids,xy)}
 for g,v in pos.items():coords.append({'Analysis':a,'GO':g,'Cluster':cluster,'x':v[0],'y':v[1]})
 ax.set_aspect('equal');ax.set_xlim(-1.25,1.25);ax.set_ylim(-1.18,1.22);ax.axis('off')
 if len(ids)>1:
  ax.add_patch(Ellipse((0,0),2.43,2.22,facecolor='#f8f8f8',edgecolor='#c7c7c7',lw=.55,zorder=0))
  ax.add_collection(LineCollection([[pos[r.source],pos[r.target]] for r in e.itertuples()],colors='#8a8a8a',linewidths=.5,alpha=.55,zorder=1))
 sizes=12+62*np.sqrt(n['size'].to_numpy()/500)
 ax.scatter(xy[:,0],xy[:,1],s=sizes,c=n.NES,cmap=cmap,norm=norm,edgecolor='white',linewidth=.35,zorder=3)
 selected=T[(T.Analysis==a)&T.GO.isin(ids)]
 for r in selected.itertuples():
  k=ids.index(r.GO);x,y=pos[r.GO]
  ax.scatter([x],[y],s=sizes[k]+24,facecolors='none',edgecolors='#202020',linewidth=.9,zorder=4)
  ax.annotate(str(int(r.Term_number)),(x,y),xytext=(3,3),textcoords='offset points',fontsize=6.5,weight='bold',bbox=dict(facecolor='white',edgecolor='none',pad=.3,alpha=.95),zorder=5)
 cr=C[(C.Analysis==a)&(C.Cluster==cluster)].iloc[0]
 label=textwrap.fill(cr.Label,25,break_long_words=False)
 ax.set_title(f'{display_letter}  {label}',fontsize=7.2,pad=5,linespacing=1.05)
 ax.text(.5,-.025,f'{len(ids)} GO terms',transform=ax.transAxes,ha='center',fontsize=6.5,color='#555555')

fig=plt.figure(figsize=(11.8,14.6))
outer=fig.add_gridspec(2,2,left=.04,right=.97,top=.963,bottom=.065,wspace=.16,hspace=.18)
for i,a in enumerate(order):
 n=N[N.Analysis==a];c=C[C.Analysis==a];t=T[T.Analysis==a].copy()
 t['abs']=t.NES.abs();t=pd.concat([t[t.NES>0].sort_values('abs',ascending=False),t[t.NES<0].sort_values('abs',ascending=False)])
 t['Term_number']=range(1,len(t)+1)
 T.loc[t.index,'Term_number']=t.Term_number
 chosen=t.Cluster.drop_duplicates().tolist();letters={cid:chr(65+j) for j,cid in enumerate(chosen)}
 T.loc[t.index,'Display_cluster']=t.Cluster.map(letters)
 sub=outer[i//2,i%2].subgridspec(2,1,height_ratios=[1.02,1.08],hspace=.12)
 at=fig.add_subplot(sub[0]);at.set_xlim(0,1);at.set_ylim(0,1);at.axis('off')
 tissue='Stem and leaves' if a.startswith('Shoot') else 'Roots';pair='CK + PE0.5' if 'PE0.5' in a else 'CK + PVC5'
 at.text(-.025,1.13,chr(97+i),fontsize=13,weight='bold',transform=at.transAxes,va='top')
 at.text(.035,1.13,f'{tissue} | {pair}',fontsize=10,weight='bold',va='top',transform=at.transAxes)
 at.text(.035,1.055,f'n = 8   |   {len(n)} significant GO terms   |   {len(c)} clusters',fontsize=7.8,transform=at.transAxes,va='top')
 at.text(0,.975,'GO biological process',fontsize=8,weight='bold',va='top');at.text(.87,.975,'NES',ha='right',fontsize=8,weight='bold',va='top');at.text(1,.975,'q',ha='right',fontsize=8,weight='bold',va='top')
 at.plot([0,1],[.93,.93],color='#999999',lw=.5)
 for j,r in enumerate(t.itertuples()):
  y=.877-j*.084;col=red if r.NES>0 else blue
  label=f'{j+1} [{letters[r.Cluster]}]  '+r.Description
  at.text(.005,y,textwrap.fill(label,43,break_long_words=False,break_on_hyphens=False),fontsize=7.7,va='center',color=col,linespacing=1.0)
  at.text(.87,y,f'{r.NES:+.2f}',ha='right',va='center',fontsize=7.7,color=col)
  q=r.Original_BH_P;qs=f'{q:.1e}' if q<.001 else f'{q:.3f}'
  at.text(1,y,qs,ha='right',va='center',fontsize=7.3)
 # These are complete clusters containing the displayed representatives, not a reclustered subset.
 cols=3 if len(chosen)>4 else 2;rows=math.ceil(len(chosen)/cols)
 ng=sub[1].subgridspec(rows,cols,wspace=.14,hspace=.98)
 for j,cid in enumerate(chosen):
  ax=fig.add_subplot(ng[j//cols,j%cols]);cluster_network(ax,a,cid,letters[cid])
 shown=N[(N.Analysis==a)&N.Cluster.isin(chosen)]
 summary.append(dict(Analysis=a,Significant_terms=len(n),Retained_terms=int((pd.read_csv(B/'data/GO_BP_retained_representatives.csv').Analysis==a).sum()),Displayed_terms=len(t),All_clusters=len(c),Displayed_clusters=len(chosen),Terms_in_displayed_clusters=len(shown),Displayed_cluster_GO_coverage=len(shown)/len(n)))
# One shared colour key for the same biomass-associated NES.
cax=fig.add_axes([.765,.026,.19,.009]);cb=fig.colorbar(plt.cm.ScalarMappable(norm=norm,cmap=cmap),cax=cax,orientation='horizontal',ticks=[-3,0,3]);cb.outline.set_visible(False);cb.ax.tick_params(labelsize=7,length=2);cb.set_label('Biomass-associated NES',fontsize=8,labelpad=3)
fig.text(.04,.035,'Node: GO term; size: gene-set size. Edge: shared genes.\nOutlined numbered nodes: terms listed above. Cluster labels: AutoAnnotate / WordCloud.',fontsize=7.3,va='center')
for ext in ['svg','tiff']:
 kw={'dpi':400}
 if ext=='tiff':kw['pil_kwargs']={'compression':'tiff_lzw'}
 fig.savefig(F/f'GO_biomass_functional_clusters.{ext}',**kw)
plt.close(fig)
T.to_csv(R/'displayed_terms_with_clusters.csv',index=False);pd.DataFrame(summary).to_csv(R/'display_summary.csv',index=False);pd.DataFrame(coords).to_csv(R/'display_coordinates.csv',index=False)
print(pd.DataFrame(summary).to_string(index=False))
)----",
    r"----(EnrichmentMap_AutoAnnotate/prepare_P_variant.py)----" = r"----(from pathlib import Path as _Fig5Path
def _fig5_root():
    return next(p for p in _Fig5Path(__file__).resolve().parents if all((p / folder).is_dir() for folder in ('code', 'data', 'figure')))
def _fig5_analysis(relative):
    return _fig5_root() / 'data' / relative.removeprefix('GO_biomass_GSEA/')
def _fig5_figures(relative):
    p = _fig5_root() / 'figure' / relative
    p.mkdir(parents=True, exist_ok=True)
    return p
from pathlib import Path
import pandas as pd
B=_fig5_analysis('GO_biomass_GSEA/EnrichmentMap_AutoAnnotate');R=B/'results'
r=pd.read_csv(B/'data/GO_BP_retained_representatives.csv');nodes=pd.read_csv(R/'all_nodes.csv');old=pd.read_csv(R/'displayed_terms.csv')
parts=[]
for a in old.Analysis.drop_duplicates():
 for sign in [1,-1]:
  z=r[(r.Analysis==a)&(r.NES*sign>0)&(r.Original_BH_P<.05)].copy();z['abs_NES']=z.NES.abs()
  z=z.sort_values(['Original_P','abs_NES','Original_BH_P','GO'],ascending=[True,False,True,True]).head(5)
  z['Display_rank']=range(1,len(z)+1);z['Sign']='Positive' if sign==1 else 'Negative';parts.append(z)
p=pd.concat(parts).merge(nodes[['Analysis','GO','Cluster']],on=['Analysis','GO'],validate='one_to_one')
assert len(p)==40 and p.Cluster.notna().all() and (p.Original_BH_P<.05).all()
p.to_csv(R/'displayed_terms_P.csv',index=False)
print('P variant: 40 retained significant terms; fixed original network and clustering.')
)----",
    r"----(EnrichmentMap_AutoAnnotate/prepare_inputs.py)----" = r"----(from pathlib import Path as _Fig5Path
def _fig5_root():
    return next(p for p in _Fig5Path(__file__).resolve().parents if all((p / folder).is_dir() for folder in ('code', 'data', 'figure')))
def _fig5_analysis(relative):
    return _fig5_root() / 'data' / relative.removeprefix('GO_biomass_GSEA/')
def _fig5_figures(relative):
    p = _fig5_root() / 'figure' / relative
    p.mkdir(parents=True, exist_ok=True)
    return p
from pathlib import Path
import pandas as pd,json,hashlib,shutil
ROOT=_fig5_analysis('GO_biomass_GSEA/EnrichmentMap_AutoAnnotate')
SRC=_fig5_root()/'data'
old=SRC/'Functional_cluster_preview/data'
D=ROOT/'data';D.mkdir(exist_ok=True)
for name in ['significant_BP_original.csv','matched_gene_sets.json','original_GSEA_methods.json']:
 shutil.copy2(old/name,D/name)
assert (D/'GO_BP_retained_representatives.csv').is_file(), 'Missing retained GO representative input'
full=pd.read_csv(D/'significant_BP_original.csv');ret=pd.read_csv(D/'GO_BP_retained_representatives.csv');sets=json.loads((D/'matched_gene_sets.json').read_text())
order=list(sets);selected=[]
for a in order:
 z=full[full.Analysis==a];b=ROOT/'cytoscape'/a;b.mkdir(parents=True,exist_ok=True)
 with (b/'genesets.gmt').open('w') as f:
  for r in z.itertuples():
   gs=sets[a][r.GO];assert len(gs)==r.size
   f.write('\t'.join([r.GO,r.Description]+gs)+'\n')
 # Generic file positional format supported by EnrichmentMap.
 out=pd.DataFrame({'GO':z.GO,'Description':z.Description,'pvalue':z.pval,'FDR':z.padj,'Phenotype':z.NES.apply(lambda v:1 if v>0 else -1)})
 out.to_csv(b/'enrichment.tsv',sep='\t',index=False)
 for sign in [1,-1]:
  zz=ret[(ret.Analysis==a)&(ret.NES*sign>0)].copy();zz['abs_NES']=zz.NES.abs()
  zz=zz.sort_values(['abs_NES','Original_BH_P','Original_P','GO'],ascending=[False,True,True,True]).head(5)
  zz['Display_rank']=range(1,len(zz)+1);zz['Sign']='Positive' if sign==1 else 'Negative';selected.append(zz)
chosen=pd.concat(selected);assert len(chosen)==40
for r in chosen.itertuples():
 x=full[(full.Analysis==r.Analysis)&(full.GO==r.GO)].iloc[0]
 assert abs(x.NES-r.NES)<1e-12 and abs(x.padj-r.Original_BH_P)<1e-12 and x.padj<.05
chosen.to_csv(ROOT/'results/displayed_terms.csv',index=False)
print('Validated: 1563 significant records, 527 retained records, 40 displayed terms; original statistics preserved.')
)----",
    r"----(EnrichmentMap_AutoAnnotate/run_cytoscape.py)----" = r"----(from cyrest import ROOT,cmd,request
import pandas as pd,json
full=pd.read_csv(ROOT/'data/significant_BP_original.csv');T=pd.read_csv(ROOT/'results/displayed_terms.csv');order=T.Analysis.drop_duplicates().tolist()
Ns=[];Es=[];Cs=[];nets={}
for i,a in enumerate(order):
 b=ROOT/'cytoscape'/a
 before=set(request('/networks'))
 cmd('enrichmentmap','build',{'analysisType':'generic','gmtFile':str(b/'genesets.gmt'),'enrichmentsDataset1':str(b/'enrichment.tsv'),'pvalue':'1.0','qvalue':'0.05','coefficients':'COMBINED','combinedConstant':'0.5','similaritycutoff':'0.375','networkName':a,'runAutoAnnotate':'false'})
 new=set(request('/networks'))-before;assert len(new)==1;netid=new.pop()
 r=cmd('autoannotate','annotate-clusterBoosted',{'network':str(netid),'clusterAlgorithm':'MCL','labelColumn':'EnrichmentMap::GS_DESCR','edgeWeightColumn':'EnrichmentMap::similarity_coefficient','maxWords':'3','adjacentWordBonus':'8','minWordOccurrence':'1','createSingletonClusters':'true','returnJsonOnly':'false','useClusterMaker':'true'})
 (b/'autoannotate.json').write_text(json.dumps(r,indent=2))
 nets[a]=netid;net=request('/networks/'+str(netid));(b/'network.json').write_text(json.dumps(net))
 raw=net['elements']['nodes'];edges=net['elements']['edges'];d=full[full.Analysis==a].copy();valid=set(d.GO)
 nodemap={int(x['data']['SUID']):x['data']['name'] for x in raw if x['data']['name'] in valid}
 assert set(nodemap.values())==valid
 clusters=r['data'];flat=[g for c in clusters for g in c['nodes']];assert len(flat)==len(set(flat))==len(d) and set(flat)==set(nodemap)
 clusters=sorted(clusters,key=lambda c:(-len(c['nodes']),min(nodemap[x] for x in c['nodes'])))
 gm={}
 for j,c in enumerate(clusters,1):
  cid=f'C{j:03d}';ids=[nodemap[x] for x in c['nodes']];gm.update({g:cid for g in ids})
  z=d[d.GO.isin(ids)]
  Cs.append({'Analysis':a,'Cluster':cid,'Label':c['label'],'Terms':len(ids),'Positive':int((z.NES>0).sum()),'Negative':int((z.NES<0).sum()),'GO_members':';'.join(sorted(ids))})
 d['Cluster']=d.GO.map(gm);assert d.Cluster.notna().all();Ns.append(d)
 for x in edges:
  e=x['data'];u=int(e['source']);v=int(e['target'])
  if u not in nodemap or v not in nodemap:continue
  Es.append({'Analysis':a,'source':nodemap[u],'target':nodemap[v],'Combined':e['EnrichmentMap_similarity_coefficient'],'Shared_genes':e['EnrichmentMap_Overlap_size']})
 # Preserve the actual continuous NES in the native session (Generic format imports only its sign).
 cols=request(f'/networks/{netid}/tables/defaultnode/columns')
 nescol=next(x['name'] for x in cols if '::NES' in x['name'])
 rows=[{'name':g,nescol:float(d.set_index('GO').loc[g,'NES'])} for g in valid]
 request(f'/networks/{netid}/tables/defaultnode',{'key':'name','dataKey':'name','data':rows},'PUT')
 (b/'network.json').write_text(json.dumps(request('/networks/'+str(netid))))
 print(a,'nodes',len(d),'edges',sum(x['Analysis']==a for x in Es),'clusters',len(clusters),flush=True)
N=pd.concat(Ns);N.to_csv(ROOT/'results/all_nodes.csv',index=False);pd.DataFrame(Es).to_csv(ROOT/'results/all_edges.csv',index=False);pd.DataFrame(Cs).to_csv(ROOT/'results/clusters.csv',index=False)
T=T.drop(columns=['Cluster'],errors='ignore').merge(N[['Analysis','GO','Cluster']],on=['Analysis','GO'],validate='one_to_one');assert T.Cluster.notna().all();T.to_csv(ROOT/'results/displayed_terms.csv',index=False)
(ROOT/'cytoscape/network_ids.json').write_text(json.dumps(nets,indent=2))
cmd('session','save as',{'file':str(ROOT/'cytoscape/GO_biomass_EnrichmentMap.cys')})
print('Saved native session',flush=True)
)----",
    r"----(Functional_cluster_preview/build_clusters.py)----" = r"----(from pathlib import Path as _Fig5Path
def _fig5_root():
    return next(p for p in _Fig5Path(__file__).resolve().parents if all((p / folder).is_dir() for folder in ('code', 'data', 'figure')))
def _fig5_analysis(relative):
    return _fig5_root() / 'data' / relative.removeprefix('GO_biomass_GSEA/')
def _fig5_figures(relative):
    p = _fig5_root() / 'figure' / relative
    p.mkdir(parents=True, exist_ok=True)
    return p
from pathlib import Path
import json, hashlib, importlib.metadata
import numpy as np
import pandas as pd
import scipy.sparse as sp
import networkx as nx
import markov_clustering as mc

BASE=_fig5_analysis('GO_biomass_GSEA/Functional_cluster_preview')
R=BASE/'results';R.mkdir(exist_ok=True)
df=pd.read_csv(BASE/'data/significant_BP_original.csv')
sets=json.loads((BASE/'data/matched_gene_sets.json').read_text())
order=['Shoot__PE0.5_vs_CK','Shoot__PVC5_vs_CK','Root__PE0.5_vs_CK','Root__PVC5_vs_CK']
allnodes=[]; alledges=[]; allclusters=[]; selected=[]; summary=[]
for ai,a in enumerate(order):
 d=df[df.Analysis==a].sort_values('GO').reset_index(drop=True).copy()
 ids=d.GO.tolist(); S=[set(sets[a][g]) for g in ids]
 assert all(len(s)==int(n) for s,n in zip(S,d['size']))
 n=len(ids); mat=np.zeros((n,n)); E=[]
 for i in range(n):
  for j in range(i+1,n):
   k=len(S[i]&S[j])
   if not k:continue
   jac=k/len(S[i]|S[j]); overlap=k/min(len(S[i]),len(S[j])); sim=(jac+overlap)/2
   if sim>0.375:
    mat[i,j]=mat[j,i]=sim
    E.append(dict(Analysis=a,source=ids[i],target=ids[j],shared_genes=k,Jaccard=jac,Overlap=overlap,Combined=sim))
 result=mc.run_mcl(sp.csr_matrix(mat),expansion=2,inflation=2,loop_value=1,iterations=1000,pruning_threshold=0.001)
 # Test convergence using one additional inflation / expansion step, including pruning.
 nxt=mc.normalize(mc.prune(mc.iterate(result,expansion=2,inflation=2),threshold=0.001))
 converged=mc.converged(nxt,result)
 assert converged, f'MCL not converged: {a}'
 clusters=mc.get_clusters(result)
 flat=[i for c in clusters for i in c]
 assert sorted(flat)==list(range(n)), 'Cluster membership must be unique and complete'
 clusters=sorted(clusters,key=lambda c:(-len(c),min(ids[i] for i in c)))
 d['Cluster']=''; d['Displayed_cluster']=False
 cs=[]
 for k,c in enumerate(clusters,1):
  cid=f'C{k:02d}'; sub=d.iloc[list(c)].copy()
  anchor=sub.sort_values(['pval','GO']).iloc[0]
  d.loc[list(c),'Cluster']=cid;d.loc[list(c),'Displayed_cluster']=True
  cs.append(dict(Analysis=a,Cluster=cid,n_terms=len(c),positive=int((sub.NES>0).sum()),negative=int((sub.NES<0).sum()),anchor_GO=anchor.GO,anchor_label=anchor.Description,min_p=float(sub.pval.min()),min_q=float(sub.padj.min()),displayed=True))
  # At most one representative per direction in each fixed displayed cluster.
  if True:
   for sign in [1,-1]:
    z=sub[sub.NES*sign>0].copy();z['abs_NES']=z.NES.abs()
    if z.empty:continue
    for variant,cols,asc in [('P',['pval','abs_NES','GO'],[True,False,True]),('NES',['abs_NES','pval','GO'],[False,True,True])]:
     rr=z.sort_values(cols,ascending=asc).iloc[0]
     selected.append(dict(Analysis=a,Variant=variant,Cluster=cid,GO=rr.GO,Description=rr.Description,NES=rr.NES,pval=rr.pval,padj=rr.padj,Direction='Positive' if sign==1 else 'Negative',size=rr['size']))
 allnodes.append(d);alledges.extend(E);allclusters.extend(cs)
 summary.append(dict(Analysis=a,significant_terms=n,positive=int((d.NES>0).sum()),negative=int((d.NES<0).sum()),edges=len(E),clusters=len(clusters),displayed_clusters=len(clusters),displayed_terms=int(d.Displayed_cluster.sum()),displayed_fraction=float(d.Displayed_cluster.mean()),converged=bool(converged)))
 print(a,summary[-1],flush=True)
 print(pd.DataFrame(cs).head(5)[['Cluster','n_terms','positive','negative','anchor_label']].to_string(index=False),flush=True)
pd.concat(allnodes).to_csv(R/'all_significant_nodes_and_clusters.csv',index=False)
pd.DataFrame(alledges).to_csv(R/'all_overlap_edges.csv',index=False)
pd.DataFrame(allclusters).to_csv(R/'all_cluster_membership_summary.csv',index=False)
candidates=pd.DataFrame(selected)
chosen=[]
for a in order:
 for v in ['P','NES']:
  for sign in ['Positive','Negative']:
   z=candidates[(candidates.Analysis==a)&(candidates.Variant==v)&(candidates.Direction==sign)].copy()
   z['abs_NES']=z.NES.abs()
   z=z.sort_values(['pval','GO'] if v=='P' else ['abs_NES','GO'],ascending=[True,True] if v=='P' else [False,True]).head(5)
   chosen.append(z)
pd.concat(chosen).to_csv(R/'displayed_representatives_P_and_NES.csv',index=False)
candidates.to_csv(R/'every_cluster_direction_candidate_P_and_NES.csv',index=False)
pd.DataFrame(summary).to_csv(R/'analysis_summary.csv',index=False)
# Also provide pure global top-five lists for comparison, without changing displayed clusters.
globalrows=[]
for a in order:
 for sign in [1,-1]:
  z=df[(df.Analysis==a)&(df.NES*sign>0)].copy();z['abs_NES']=z.NES.abs()
  for variant,cols,asc in [('P',['pval','abs_NES','GO'],[True,False,True]),('NES',['abs_NES','pval','GO'],[False,True,True])]:
   t=z.sort_values(cols,ascending=asc).head(5).copy();t['Variant']=variant;t['Rank']=range(1,len(t)+1);globalrows.append(t)
pd.concat(globalrows).drop(columns=['leadingEdge']).to_csv(R/'global_top5_each_direction_comparison.csv',index=False)
params=dict(significance='Original BH q < 0.05, separately per analysis',network_input='All original significant BP terms before collapsePathways; complete gene sets intersected with each original ranking universe',edge='Combined=(Jaccard+Overlap)/2 >0.375',MCL=dict(implementation='Python markov_clustering; not Cytoscape / AutoAnnotate execution',inflation=2,expansion=2,loop_value=1,pruning_threshold=.001,max_iterations=1000),display='All significant terms and all MCL clusters displayed. Cluster numbers ranked by size with GO-ID tie-breaker. Representative lists contain five terms per sign, at most one per cluster per sign, selected by P or |NES|. This cluster-balanced display rule is our explicit adaptation, not an asserted rule of Kim et al.',representatives='Up to five terms per NES sign, at most one per cluster per sign: smallest raw P versus largest absolute NES; ties deterministically broken. Same nodes, edges, layout and cluster selection in both versions.',cluster_label='Name of smallest-P term, used as an anchor, NOT an official GO category or whole-cluster enrichment result',seed=20261028,references=['https://www.nature.com/articles/s41467-022-32748-5','https://www.nature.com/articles/s41596-018-0103-9/figures/10','https://baderlab.github.io/Cytoscape_workflows/EnrichmentMapPipeline/Protocol2_annotateEM.html','https://markov-clustering.readthedocs.io/en/latest/readme.html'],versions={p:importlib.metadata.version(p) for p in ['numpy','pandas','scipy','networkx','markov_clustering','matplotlib']})
(BASE/'parameters.json').write_text(json.dumps(params,indent=2,ensure_ascii=False))
)----",
    r"----(Functional_cluster_preview/plot_previews.py)----" = r"----(from pathlib import Path as _Fig5Path
def _fig5_root():
    return next(p for p in _Fig5Path(__file__).resolve().parents if all((p / folder).is_dir() for folder in ('code', 'data', 'figure')))
def _fig5_analysis(relative):
    return _fig5_root() / 'data' / relative.removeprefix('GO_biomass_GSEA/')
def _fig5_figures(relative):
    p = _fig5_root() / 'figure' / relative
    p.mkdir(parents=True, exist_ok=True)
    return p
from pathlib import Path
import json, textwrap, html, hashlib, os
import numpy as np
import pandas as pd
import networkx as nx
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from matplotlib import colors, font_manager
from matplotlib.collections import LineCollection
from matplotlib.patches import Ellipse

BASE=_fig5_analysis('GO_biomass_GSEA/Functional_cluster_preview');R=BASE/'results';F=_fig5_figures('Functional_cluster_preview');F.mkdir(exist_ok=True)
# Use actual Arial font files, fail rather than silently substitute.
font=font_manager.findfont('Arial',fallback_to_default=False)
plt.rcParams.update({'font.family':'Arial','font.size':9,'axes.linewidth':.6,'pdf.fonttype':42,'ps.fonttype':42,'svg.fonttype':'none','savefig.facecolor':'white'})
N=pd.read_csv(R/'all_significant_nodes_and_clusters.csv');E=pd.read_csv(R/'all_overlap_edges.csv');C=pd.read_csv(R/'all_cluster_membership_summary.csv');T=pd.read_csv(R/'displayed_representatives_P_and_NES.csv');S=pd.read_csv(R/'analysis_summary.csv')
order=S.Analysis.tolist()
titles=['Shoots | CK + PE0.5','Shoots | CK + PVC5','Roots | CK + PE0.5','Roots | CK + PVC5']
red='#D6503A';blue='#286FA8';cmap=colors.LinearSegmentedColormap.from_list('biomass',[(0,blue),(.30,'#5e97c5'),(.5,'#f8f8f8'),(.70,'#df7b68'),(1,red)]);norm=colors.TwoSlopeNorm(vmin=-3.5,vcenter=0,vmax=3.5)
# Identical deterministic positions for P and NES versions. Separate cluster anchors;
# all edges among the displayed nodes are retained, including between clusters.
positions={}
fixed=pd.read_csv(R/'fixed_network_coordinates.csv')
assert not fixed.duplicated(['Analysis','GO']).any()
for a in order:
 n=N[N.Analysis==a].set_index('GO');e=E[E.Analysis==a];c=C[C.Analysis==a]
 G=nx.Graph();G.add_nodes_from(n.index)
 for row in e.itertuples():G.add_edge(row.source,row.target,weight=row.Combined)
 coords=fixed[fixed.Analysis==a].set_index('GO')
 assert set(coords.index)==set(n.index)
 pos={g:coords.loc[g,['x','y']].to_numpy(dtype=float) for g in n.index}
 discs=[]
 for row in c.itertuples():
  members=n[n.Cluster==row.Cluster].index
  centre=np.mean([pos[g] for g in members],axis=0)
  discs.append(dict(Cluster=row.Cluster,centre=centre,radius=.20+.14*np.sqrt(row.n_terms)))
 positions[a]=(G,pos,discs)

def sci(x):
 return f'{x:.1e}' if x<.001 else f'{x:.3f}'

def table(ax,a,variant):
 z=T[(T.Analysis==a)&(T.Variant==variant)].copy();z['abs']=z.NES.abs()
 chunks=[]
 for sign in ['Positive','Negative']:
  sub=z[z.Direction==sign].sort_values(['pval','GO'] if variant=='P' else ['abs','GO'],ascending=[True,True] if variant=='P' else [False,True])
  chunks.append(sub)
 rows=pd.concat(chunks)
 ax.set_xlim(0,1);ax.set_ylim(0,1);ax.axis('off')
 ax.text(.00,.98,'GO biological process',weight='bold',va='top',fontsize=9)
 for x,label in [(.69,'NES'),(.82,'P'),(.965,'q')]:ax.text(x,.98,label,weight='bold',ha='right',va='top',fontsize=9)
 ax.plot([0,1],[.925,.925],color='#bbbbbb',lw=.6)
 # Fixed ten-row capacity keeps all four panels directly comparable.
 y=.887;step=.081
 for r in rows.itertuples():
  col=red if r.NES>0 else blue
  label=f'{r.Cluster}  {r.Description}'
  wrapped=textwrap.fill(label,47,break_long_words=False,break_on_hyphens=False)
  ax.text(.005,y,wrapped,fontsize=8.7,ha='left',va='center',linespacing=1.03,color=col)
  ax.text(.69,y,f'{r.NES:+.2f}',ha='right',va='center',fontsize=8.7,color=col)
  ax.text(.82,y,sci(r.pval),ha='right',va='center',fontsize=8.4)
  ax.text(.965,y,sci(r.padj),ha='right',va='center',fontsize=8.4)
  y-=step
 ax.text(0,.015,('Representative selection: smallest P' if variant=='P' else 'Representative selection: largest |NES|'),fontsize=8,color='#555555',va='bottom')

def network(ax,a):
 n=N[N.Analysis==a].set_index('GO');G,pos,discs=positions[a]
 selected_clusters=set(T[T.Analysis==a].Cluster)
 ax.set_aspect('equal');ax.axis('off')
 bounds=[]
 for disk in discs:
  cx,cy=disk['centre'];r=disk['radius'];cid=disk['Cluster']
  ax.add_patch(Ellipse((cx,cy),2*r,2*r,facecolor='#f6f6f6',edgecolor='#d3d3d3',lw=.4,zorder=0))
  bounds.extend([[cx-r-.1,cy-r-.1],[cx+r+.1,cy+r+.1]])
  if cid in selected_clusters:
   ax.text(cx,cy+r-.02,cid,ha='center',va='bottom',fontsize=7.2,weight='bold',color='#333333',zorder=5,bbox=dict(facecolor='white',edgecolor='none',pad=.25,alpha=.94))
 edges=list(G.edges());segs=[[pos[u],pos[v]] for u,v in edges]
 ax.add_collection(LineCollection(segs,colors='#929292',linewidths=[.12+.30*G[u][v]['weight'] for u,v in edges],alpha=.35,zorder=1))
 ns=list(G.nodes());xy=np.array([pos[g] for g in ns]);q=n.loc[ns,'padj'].values
 size=9+1.0*np.minimum(-np.log10(q),40)
 ax.scatter(xy[:,0],xy[:,1],s=size,c=n.loc[ns,'NES'],cmap=cmap,norm=norm,edgecolor='white',linewidth=.18,zorder=3)
 # Same highlight union across both ranking versions.
 chosen=set(T[T.Analysis==a].GO)
 ix=[g for g in ns if g in chosen];hi=np.array([pos[g] for g in ix])
 ax.scatter(hi[:,0],hi[:,1],s=16+1.0*np.minimum(-np.log10(n.loc[ix,'padj']),40),facecolor='none',edgecolor='#222222',linewidth=.5,zorder=4)
 bb=np.array(bounds);ax.set_xlim(bb[:,0].min(),bb[:,0].max());ax.set_ylim(bb[:,1].min(),bb[:,1].max())

for variant in os.environ.get('FUNCTIONAL_CLUSTER_VARIANTS','P,NES').split(','):
 fig=plt.figure(figsize=(16,17.6))
 outer=fig.add_gridspec(2,2,left=.035,right=.985,top=.95,bottom=.065,wspace=.12,hspace=.16)
 for i,a in enumerate(order):
  grid=outer[i//2,i%2].subgridspec(2,1,height_ratios=[1,1.08],hspace=.10)
  at=fig.add_subplot(grid[0]);an=fig.add_subplot(grid[1]);table(at,a,variant);network(an,a)
  at.text(-.025,1.16,chr(97+i),transform=at.transAxes,fontsize=15,weight='bold',va='top')
  at.text(.04,1.16,titles[i],transform=at.transAxes,fontsize=12,weight='bold',va='top')
  st=S[S.Analysis==a].iloc[0]
  at.text(.04,1.085,f'Biomass association · n = 8 · {st.significant_terms} significant GO terms',transform=at.transAxes,fontsize=9.5,va='top')
  an.text(.5,-.065,f'All {st.significant_terms} significant terms · {st.clusters} functional clusters',transform=an.transAxes,ha='center',fontsize=8.5,color='#555555')
 # Shared legend, no implication of treatment up/down.
 ca=fig.add_axes([.38,.027,.24,.010]);cb=fig.colorbar(plt.cm.ScalarMappable(norm=norm,cmap=cmap),cax=ca,orientation='horizontal',ticks=[-3,-1.5,0,1.5,3]);cb.set_label('Biomass-associated NES',fontsize=9,labelpad=3);cb.ax.tick_params(labelsize=8,length=2);cb.outline.set_visible(False)
 fig.text(.035,.039,'Node: GO term   |   Edge: shared genes\nNode size: significance, -log10(q)',fontsize=8.5,va='center')
 fig.text(.985,.039,'Red: positive biomass association\nBlue: negative biomass association',fontsize=8.5,ha='right',va='center')
 stem=F/f'GO_biomass_clusters_rank_{variant}'
 qa=os.environ.get('FUNCTIONAL_CLUSTER_QA','')
 if qa:
  Path(qa).mkdir(parents=True,exist_ok=True)
  helpers=os.environ.get('FUNCTIONAL_CLUSTER_ALIGNMENT_HELPERS','')
  if helpers:
   import sys
   sys.path.insert(0,helpers)
   from audit_panel_alignment import require_matplotlib_panel_alignment
   # Network panels keep equal aspect; compare the four aligned table containers.
   require_matplotlib_panel_alignment(fig,axes=[fig.axes[j] for j in [0,2,4,6]],
    panel_ids=list('abcd'),row_groups=[['a','b'],['c','d']],column_groups=[['a','c'],['b','d']],
    json_out=Path(qa)/(stem.name+'.alignment.json'),strict=True)
  fig.savefig(Path(qa)/(stem.name+'.pdf'))
  fig.savefig(Path(qa)/(stem.name+'.png'),dpi=120)
 for ext in ['svg']:
  fig.savefig(str(stem)+'.'+ext,dpi=300)
 fig.savefig(str(stem)+'.tiff',dpi=300,pil_kwargs={'compression':'tiff_lzw'})
 plt.close(fig)
 print('Rendered',variant,flush=True)
# All-cluster browsing is tabular, searchable and complete; no extra inference is introduced.
cluster_table=C.to_html(index=False,escape=True,classes='data')
summary=S.to_html(index=False,classes='data')
comp=[]
for a in order:
 p=T[(T.Analysis==a)&(T.Variant=='P')];m=T[(T.Analysis==a)&(T.Variant=='NES')]
 comp.append(dict(Analysis=a,P_terms=len(p),NES_terms=len(m),Shared_terms=len(set(p.GO)&set(m.GO)),P_only=';'.join(sorted(set(p.GO)-set(m.GO))),NES_only=';'.join(sorted(set(m.GO)-set(p.GO)))))
compare=pd.DataFrame(comp);compare.to_csv(R/'representative_selection_comparison.csv',index=False)
print(compare.to_string(index=False),flush=True)
)----",
    r"----(Functional_cluster_preview/requirements.txt)----" = r"----(cloudpickle==3.1.2
contourpy==1.4.0
cycler==0.12.1
fonttools==4.68.0
joblib==1.6.0
kiwisolver==1.5.1
markov-clustering==0.0.6.dev0
matplotlib==3.11.2
narwhals==2.28.0
networkx==3.7
numpy==2.5.4
packaging==26.3
pandas==3.1.0
pillow==13.0.0
pyparsing==3.3.3
python-dateutil==3.0.0
scikit-learn==1.9.1
scipy==1.18.1
six==1.17.0
threadpoolctl==3.7.0
tzdata==2026.5
)----"
  )
  for(name in names(code_files)) {
    destination<-file.path(stage,'code',name);dir.create(dirname(destination),recursive=TRUE,showWarnings=FALSE)
    writeLines(code_files[[name]],destination,useBytes=TRUE)
  }
  if('--extract-only'%in%args){message('Validated inputs and plotting code extracted.');return(invisible(TRUE))}
  candidates<-unique(c(arg('--python=',Sys.getenv('FUNCTIONAL_CLUSTER_PYTHON')),
    Sys.which('python3'),file.path(path.expand('~'),'.cache/codex-runtimes/codex-primary-runtime/dependencies/python/bin/python3')))
  dependency_check<-if(mode=='rebuild-preview') 'import numpy,pandas,networkx,matplotlib.pyplot,PIL,scipy,markov_clustering' else 'import numpy,pandas,networkx,matplotlib.pyplot,PIL'
  python<-NULL
  for(candidate in candidates[nzchar(candidates)]) if(file.exists(candidate)) {
    ok<-suppressWarnings(system2(candidate,c('-c',shQuote(dependency_check)),stdout=FALSE,stderr=FALSE))
    if(identical(ok,0L)){python<-candidate;break}
  }
  if(is.null(python))stop('Set --python= to a Python interpreter with the dependencies listed at the top of this script. The workbook check does not require Python.')
  Sys.setenv(MPLCONFIGDIR=file.path(stage,'_matplotlib_cache'))
  run_python<-function(name){
    status<-system2(python,shQuote(file.path(stage,'code',name)))
    if(status!=0L)stop('Python step failed: ',name)
  }
  if(mode=='complete-nes'){
    old_variant<-Sys.getenv('FUNCTIONAL_CLUSTER_VARIANTS',unset=NA_character_)
    old_qa<-Sys.getenv('FUNCTIONAL_CLUSTER_QA',unset=NA_character_)
    on.exit({if(is.na(old_variant))Sys.unsetenv('FUNCTIONAL_CLUSTER_VARIANTS')else Sys.setenv(FUNCTIONAL_CLUSTER_VARIANTS=old_variant);if(is.na(old_qa))Sys.unsetenv('FUNCTIONAL_CLUSTER_QA')else Sys.setenv(FUNCTIONAL_CLUSTER_QA=old_qa)},add=TRUE)
    Sys.setenv(FUNCTIONAL_CLUSTER_VARIANTS='NES')
    qa<-arg('--qa-dir=','');if(nzchar(qa))Sys.setenv(FUNCTIONAL_CLUSTER_QA=qa)
    run_python('Functional_cluster_preview/plot_previews.py')
    files<-file.path(stage,'figure','Functional_cluster_preview',c('GO_biomass_clusters_rank_NES.svg','GO_biomass_clusters_rank_NES.tiff'))
    dir.create(output,recursive=TRUE,showWarnings=FALSE)
    if(!all(file.copy(files,file.path(output,basename(files)),overwrite=TRUE)))stop('Could not save complete NES figure.')
    stopifnot(identical(unname(tools::md5sum(input)),unname(original_hash)))
    return(invisible(TRUE))
  }
  if(mode=='rebuild-preview')run_python('Functional_cluster_preview/build_clusters.py')
  if(mode=='cytoscape'){
    run_python('EnrichmentMap_AutoAnnotate/prepare_inputs.py')
    run_python('EnrichmentMap_AutoAnnotate/run_cytoscape.py')
    run_python('EnrichmentMap_AutoAnnotate/Summary_network_example/build_summary.py')
  }
  if(mode!='rebuild-preview'){
    run_python('EnrichmentMap_AutoAnnotate/prepare_P_variant.py')
    run_python('EnrichmentMap_AutoAnnotate/Summary_network_example/prepare_example.py')
  }
  run_python('Functional_cluster_preview/plot_previews.py')
  run_python('EnrichmentMap_AutoAnnotate/plot_figure.py')
  run_python('EnrichmentMap_AutoAnnotate/plot_P_variant.py')
  run_python('EnrichmentMap_AutoAnnotate/Summary_network_example/plot_example.py')
  run_python('EnrichmentMap_AutoAnnotate/compare_rankings.py')
  files<-list.files(file.path(stage,'figure'),pattern='[.](tiff|svg)$',recursive=TRUE,full.names=TRUE)
  if(length(files)!=13L || anyDuplicated(basename(files)))stop('Unexpected figure output set.')
  dir.create(output,recursive=TRUE,showWarnings=FALSE)
  if(!all(file.copy(files,file.path(output,basename(files)),overwrite=TRUE)))stop('Could not save all figure files.')
  stopifnot(identical(unname(tools::md5sum(input)),unname(original_hash)))
  message('Saved 13 TIFF/SVG files to: ',normalizePath(output))
  invisible(TRUE)
}
run_functional_clusters()
