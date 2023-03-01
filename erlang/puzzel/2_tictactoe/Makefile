.PHONY: check clean
check: tictactoe.beam tests.beam
	erl -noinput -s tests run_tests

%.beam: %.erl
	erl -compile $<

clean:
	rm -rf *.beam
