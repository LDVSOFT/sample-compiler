TESTS=$(sort $(basename $(wildcard generated*.expr)))

.PHONY: check $(TESTS)

check: $(TESTS)

$(TESTS): %: %.expr
	RC_RUNTIME=../../runtime ../../src/rc.byte -o $< && ./$@ < $@.input > $@.log && diff $@.log orig/$@.log
	../../src/rc.byte    $< < $@.input > $@.log && diff $@.log orig/$@.log
	../../src/rc.byte -s $< < $@.input > $@.log && diff $@.log orig/$@.log

clean:
	rm -f *.log *.s *~ $(TESTS)
