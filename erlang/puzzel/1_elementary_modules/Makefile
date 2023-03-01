.PHONY: tests pi users shopping mergesort clean
tests: pi users shopping mergesort

pi: pi.beam test_pi.beam
	erl -noinput -s test_pi run_tests

users: users.beam test_users.beam
	erl -noinput -s test_users run_tests

shopping: shopping.beam test_shopping.beam
	erl -noinput -s test_shopping run_tests

mergesort: mergesort.beam test_mergesort.beam
	erl -noinput -s test_mergesort run_tests

%.beam: %.erl
	erl -compile $<

clean:
	rm -rf *.beam
