TESTS=$(sort $(basename $(wildcard test*.expr)))


.PHONY: check $(TESTS)

check: $(TESTS) 

$(TESTS): %: %.expr
	../src/rc.byte -o $< && ./$@ < $@.input > $@.log && diff $@.log orig/$@.log
	../src/rc.byte    $< < $@.input > $@.log && diff $@.log orig/$@.log
	../src/rc.byte -s $< < $@.input > $@.log && diff $@.log orig/$@.log

clean:
	rm -f test*.log *.s *~ $(TESTS)
