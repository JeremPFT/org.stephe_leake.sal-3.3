# gnu make file for SAL

ZIP_VERSION := 3.3

all : prove_all test_all_harness.diff

# export Standard_Common_Build := Debug

#../test/test_all_harness.adb
#../test/test_one_harness.adb
#t_one : export GPRBUILD_ARGS := -gnatG -gnatL
t_one : test_one_harness.run

t_all : test_all_harness.run

# --report=all to see all successfull proofs.
# -d debug; save proof files in build/obj/gnatprove/* (_not_ build/gnatprove!)
prove :
	gnatprove -P sal_test.gpr -j 0 --level 4 --no-subprojects --mode all -u prove_integer_queues.ads


trace :
	addr2line -e test_all_harness.exe 0x4ec038 0x7c9037bd 0x7c903789 0x7c90eaf8 0x4cedff 0x41a94e 0x41e79a 0x407b5c 0x40aa8e 0x4af793 0x401b66 0x401235 0x401286 0x7c816fd5

GNAT_PROJECT := sal_test.gpr

include rules.make

one_mem : exec_pro/exercise_min_heap.exe
	$< $(RUN_ARGS)
	gnatmem $<

SAL_BRANCH := $(notdir $(shell cd ..; pwd))
AUNIT_EXT_BRANCH := org.stephe_leake.aunit_ext
MAKERULES_BRANCH := org.stephe_leake.makerules

# zip includes aunit_ext, makerules; they are not big enough to deserve their own tarball.
zip :
	rm -rf ../../$(SAL_BRANCH)-$(ZIP_VERSION)
	rm -rf ../../$(AUNIT_EXT_BRANCH)-$(ZIP_VERSION)
	rm -rf ../../$(MAKERULES_BRANCH)-$(ZIP_VERSION)
	mtn checkout --branch $(SAL_BRANCH) ../../$(SAL_BRANCH)-$(ZIP_VERSION)
	mtn checkout --branch $(AUNIT_EXT_BRANCH) ../../$(AUNIT_EXT_BRANCH)-$(ZIP_VERSION)
	mtn checkout --branch $(MAKERULES_BRANCH) ../../$(MAKERULES_BRANCH)-$(ZIP_VERSION)
	tar -c -C ../.. --exclude=_MTN --exclude=.mtn-ignore $(SAL_BRANCH)-$(ZIP_VERSION) $(AUNIT_EXT_BRANCH)-$(ZIP_VERSION) $(MAKERULES_BRANCH)-$(ZIP_VERSION) | bzip2 -9 > org.stephe_leake.sal-$(ZIP_VERSION).tar.bz2

tag :
	mtn tag h:$(SAL_BRANCH) $(SAL_BRANCH)-$(ZIP_VERSION)
	mtn tag h:$(AUNIT_EXT_BRANCH) $(AUNIT_EXT_BRANCH)-$(ZIP_VERSION)
	mtn tag h:$(MAKERULES_BRANCH) $(MAKERULES_BRANCH)-$(ZIP_VERSION)

#
# Local Variables:
# eval: (unless dvc-doing-ediff-p (load-file "prj.el"))
# End:
# end of file
