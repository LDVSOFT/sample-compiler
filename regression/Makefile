TESTS=$(sort $(basename $(wildcard test*.expr)))

.PHONY: check $(TESTS)

check: $(TESTS)

$(TESTS): %: %.expr
	../rc.native -o $< && ./$@ < $@.input > $@.log && diff $@.log orig/$@.log
	../rc.native    $< < $@.input > $@.log && diff $@.log orig/$@.log
	../rc.native -s $< < $@.input > $@.log && diff $@.log orig/$@.log

clean:
	rm -f test*.log *.s *~ $(TESTS)
