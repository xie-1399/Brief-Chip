
mux2:
	vcs -full64 -cpp g++-4.8 -cc gcc-4.8 -LDFLAGS -Wl,--no-as-needed \
	-fsdb mux2_tb.v -sverilog ../hw/*.v -debug_all

sim_vcs:
	./simv

clean:
	rm -rf csrc	   \
	rm -rf simv.daidir \
	rm -rf DVEfiles    \
	rm -rf verdiLog	   \
	rm -rf simv        \
	rm -rf *.vpd       \
	rm -rf *.key       \
	rm -rf *.vdb       \
	rm -rf *.fsdb	   \
	rm -rf *.log	   \
	rm -rf *.rc 	   \
	rm -rf *.conf
