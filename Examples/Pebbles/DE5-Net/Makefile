ifndef QUARTUS_ROOTDIR
  $(error Please set QUARTUS_ROOTDIR)
endif

.PHONY: one
one: 
	qsys-generate -syn SoC.qsys
	quartus_sh --flow compile DE5.qpf

.PHONY: many
many:
	qsys-generate -syn SoC.qsys
	quartus_dse DE5.qpf    \
    --num-seeds 7               \
    --launcher local            \
    --num-concurrent 4
	quartus_dse DE5.qpf --report utilization
	quartus_dse DE5.qpf --report fmax_summary

.PHONY: report
report:
	quartus_dse DE5.qpf --report utilization
	quartus_dse DE5.qpf --report fmax_summary

.PHONY: download-sof
download-sof:
	quartus_pgm -m jtag -o "p;DE5.sof"

.PHONY: update-mif
update-mif:
	quartus_cdb --update_mif DE5.qpf
	quartus_asm DE5.qpf

.PHONY: clean
clean: clean-mif
	rm -f *.rpt *.msg *.summary *.sld *.sopcinfo *.jdi
	rm -f *.pin *.done *.qws *.sof *.csv *.qws *.smsg
	rm -rf dse* db incremental_db SoC .qsys_edit reconfig_mif SoC .qsys-edit

.PHONY: clean-mif
clean-mif:
	rm -f *.mif
