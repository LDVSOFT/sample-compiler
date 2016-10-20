TESTS=$(sort $(basename $(wildcard generated*.expr)))

.PHONY: check $(TESTS)

check: $(TESTS)

$(TESTS): %: %.expr
	RC_RUNTIME=../../runtime ../../rc.native -o $< && ./$@ < $@.input > $@.log && diff $@.log orig/$@.log
	../../rc.native    $< < $@.input > $@.log && diff $@.log orig/$@.log
	../../rc.native -s $< < $@.input > $@.log && diff $@.log orig/$@.log

clean:
	rm -f *.log *.s *~ $(TESTS)
