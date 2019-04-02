ifndef QUARTUS_ROOTDIR
  $(error Please set QUARTUS_ROOTDIR)
endif

.PHONY: one
one: 
	qsys-generate -syn SoC.qsys
	quartus_sh --flow compile DE5.qpf

.PHONY: download-sof
download-sof:
	quartus_pgm -m jtag -o "p;DE5.sof"

.PHONY: update-mif
update-mif:
	quartus_cdb --update_mif DE5.qpf
	quartus_asm DE5.qpf

.PHONY: clean
clean:
	rm -f *.mif *.rpt *.msg *.summary *.sld *.sopcinfo *.jdi
	rm -f *.pin *.done *.qws *.sof *.csv *.qws *.smsg
	rm -rf dse* db incremental_db SoC .qsys_edit reconfig_mif SoC .qsys-edit
