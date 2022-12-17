capture clear all

global prgmpath "D:\projects\skill_gaps_lead\programs\" 

/*DATA PROCESSING*/
do "${prgmpath}01_create_v01_phdcn_master.do" nostop

do "${prgmpath}02_create_v01_phdcn_demo.do" nostop

do "${prgmpath}03_create_v01_phdcn_header.do" nostop

do "${prgmpath}04_create_v01_phdcn_cbcl.do" nostop

do "${prgmpath}05_create_v01_phdcn_ppvt.do" nostop

do "${prgmpath}06_create_v01_phdcn_tractlink.do" nostop

do "${prgmpath}07_create_v01_bldb_nc.do" nostop

do "${prgmpath}08_create_v01_ncdb_nc.do" nostop

do "${prgmpath}09_create_v01_phdcn_merged.do" nostop

do "${prgmpath}10_create_v02_phdcn_merged_mi.do" nostop

/*TABLES AND FIGURES*/
do "${prgmpath}11_create_table_1.do" nostop

do "${prgmpath}12_create_table_2.do" nostop

do "${prgmpath}13_create_table_3.do" nostop

do "${prgmpath}14_create_figure_1.do" nostop

shell "D:\Program Files\R\R-4.0.2\bin\x64\R.exe" CMD BATCH --vanilla --slave --no-restore --no-timing --no-echo "${prgmpath}15_create_figure_2.R"
shell DEL "${prgmpath}15_create_figure_2.Rout"

shell "D:\Program Files\R\R-4.0.2\bin\x64\R.exe" CMD BATCH --vanilla --slave --no-restore --no-timing --no-echo "${prgmpath}16_create_figure_3.R"
shell DEL "${prgmpath}16_create_figure_3.Rout"

shell "D:\Program Files\R\R-4.0.2\bin\x64\R.exe" CMD BATCH --vanilla --slave --no-restore --no-timing --no-echo "${prgmpath}17_create_table_4.R"
shell DEL "${prgmpath}17_create_table_4.Rout"

shell "D:\Program Files\R\R-4.0.2\bin\x64\R.exe" CMD BATCH --vanilla --slave --no-restore --no-timing --no-echo "${prgmpath}18_create_table_5.R"
shell DEL "${prgmpath}18_create_table_5.Rout"

shell "D:\Program Files\R\R-4.0.2\bin\x64\R.exe" CMD BATCH --vanilla --slave --no-restore --no-timing --no-echo "${prgmpath}19_create_figure_4.R"
shell DEL "${prgmpath}19_create_figure_4.Rout"

shell "D:\Program Files\R\R-4.0.2\bin\x64\R.exe" CMD BATCH --vanilla --slave --no-restore --no-timing --no-echo "${prgmpath}20_create_figure_5.R"
shell DEL "${prgmpath}20_create_figure_5.Rout"

/*APPENDICES*/
//A
shell "D:\Program Files\R\R-4.0.2\bin\x64\R.exe" CMD BATCH --vanilla --slave --no-restore --no-timing --no-echo "${prgmpath}appendix_a\01_create_table_a1.R"
shell DEL "${prgmpath}appendix_a\01_create_table_a1.Rout"

shell "D:\Program Files\R\R-4.0.2\bin\x64\R.exe" CMD BATCH --vanilla --slave --no-restore --no-timing --no-echo "${prgmpath}appendix_a\02_create_table_a2.R"
shell DEL "${prgmpath}appendix_a\02_create_table_a2.Rout"

shell "D:\Program Files\R\R-4.0.2\bin\x64\R.exe" CMD BATCH --vanilla --slave --no-restore --no-timing --no-echo "${prgmpath}appendix_a\03_create_figure_a1.R"
shell DEL "${prgmpath}appendix_a\03_create_figure_a1.Rout"

shell "D:\Program Files\R\R-4.0.2\bin\x64\R.exe" CMD BATCH --vanilla --slave --no-restore --no-timing --no-echo "${prgmpath}appendix_a\04_create_figure_a2.R"
shell DEL "${prgmpath}appendix_a\04_create_figure_a2.Rout"

//B
shell "D:\Program Files\R\R-4.0.2\bin\x64\R.exe" CMD BATCH --vanilla --slave --no-restore --no-timing --no-echo "${prgmpath}appendix_b\01_create_table_b1.R"
shell DEL "${prgmpath}appendix_b\01_create_table_b1.Rout"

shell "D:\Program Files\R\R-4.0.2\bin\x64\R.exe" CMD BATCH --vanilla --slave --no-restore --no-timing --no-echo "${prgmpath}appendix_b\02_create_table_b2.R"
shell DEL "${prgmpath}appendix_b\02_create_table_b2.Rout"

shell "D:\Program Files\R\R-4.0.2\bin\x64\R.exe" CMD BATCH --vanilla --slave --no-restore --no-timing --no-echo "${prgmpath}appendix_b\03_create_table_b3.R"
shell DEL "${prgmpath}appendix_b\03_create_table_b3.Rout"

shell "D:\Program Files\R\R-4.0.2\bin\x64\R.exe" CMD BATCH --vanilla --slave --no-restore --no-timing --no-echo "${prgmpath}appendix_b\04_create_table_b4.R"
shell DEL "${prgmpath}appendix_b\04_create_table_b4.Rout"

shell "D:\Program Files\R\R-4.0.2\bin\x64\R.exe" CMD BATCH --vanilla --slave --no-restore --no-timing --no-echo "${prgmpath}appendix_b\05_create_table_b5.R"
shell DEL "${prgmpath}appendix_b\05_create_table_b5.Rout"

shell "D:\Program Files\R\R-4.0.2\bin\x64\R.exe" CMD BATCH --vanilla --slave --no-restore --no-timing --no-echo "${prgmpath}appendix_b\06_create_table_b6.R"
shell DEL "${prgmpath}appendix_b\06_create_table_b6.Rout"

//C
do "${prgmpath}\appendix_c\01_create_v03_phdcn_merged_mi.do" nostop

shell "D:\Program Files\R\R-4.0.2\bin\x64\R.exe" CMD BATCH --vanilla --slave --no-restore --no-timing --no-echo "${prgmpath}appendix_c\02_create_table_c1.R"
shell DEL "${prgmpath}appendix_c\02_create_table_c1.Rout"

shell "D:\Program Files\R\R-4.0.2\bin\x64\R.exe" CMD BATCH --vanilla --slave --no-restore --no-timing --no-echo "${prgmpath}appendix_c\03_create_table_c2.R"
shell DEL "${prgmpath}appendix_c\03_create_table_c2.Rout"

shell "D:\Program Files\R\R-4.0.2\bin\x64\R.exe" CMD BATCH --vanilla --slave --no-restore --no-timing --no-echo "${prgmpath}appendix_c\04_create_figure_c1.R"
shell DEL "${prgmpath}appendix_c\04_create_figure_c1.Rout"

shell "D:\Program Files\R\R-4.0.2\bin\x64\R.exe" CMD BATCH --vanilla --slave --no-restore --no-timing --no-echo "${prgmpath}appendix_c\05_create_figure_c2.R"
shell DEL "${prgmpath}appendix_c\05_create_figure_c2.Rout"

//D
shell "D:\Program Files\R\R-4.0.2\bin\x64\R.exe" CMD BATCH --vanilla --slave --no-restore --no-timing --no-echo "${prgmpath}appendix_d\01_create_table_d1.R"
shell DEL "${prgmpath}appendix_d\01_create_table_d1.Rout"

shell "D:\Program Files\R\R-4.0.2\bin\x64\R.exe" CMD BATCH --vanilla --slave --no-restore --no-timing --no-echo "${prgmpath}appendix_d\02_create_figure_d1.R"
shell DEL "${prgmpath}appendix_d\02_create_figure_d1.Rout"
