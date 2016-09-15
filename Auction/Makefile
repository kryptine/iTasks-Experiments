CPM=cpm
PROJ=Auction

default:
	$(CPM) project $(PROJ).prj build

force:
	$(CPM) project $(PROJ).prj build --force

clean:
	rm -rf "Clean System Files" $(PROJ).exe $(PROJ).prp sapl $(PROJ)-data

debug:
	cocl-tonic -P . $(PROJ).icl

rebuild:
	make clean && make

.PHONY: clean debug default force rebuild
