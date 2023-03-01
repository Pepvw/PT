TEST_SETUP=erl -noinput -s $@ tests_run 
TESTS=tests_1a tests_1b tests_2a tests_2b tests_3 tests_4 tests_5

.PHONY: clean tests ${TESTS}

tests: ${TESTS}

%.beam: %.erl
	erl -compile $<

tests_1a: tests_1a.beam grid.beam
	${TEST_SETUP}
	@echo ""

tests_1b: tests_1b.beam grid.beam
	${TEST_SETUP}
	@echo ""

tests_2a: tests_2a.beam grid.beam
	${TEST_SETUP}
	@echo ""

tests_2b: tests_2b.beam client.beam mock_game_server.beam game_server.beam grid.beam
	${TEST_SETUP}
	@echo ""

tests_3: tests_3.beam game_server.beam
	${TEST_SETUP}
	@echo ""

tests_4: tests_4.beam mock_game_server.beam mock_client.beam lobby_server.beam
	${TEST_SETUP}
	@echo ""

tests_5: tests_5.beam grid.beam
	${TEST_SETUP}

clean:
	rm -f *.beam
