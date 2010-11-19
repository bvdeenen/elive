.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

.yrl.erl:
	erlc -W $<

ERL = erl -boot start_clean
MODS = elive ball statistics_process

all: compile

compile: ${MODS:%=%.beam}

run: compile
	${ERL} -s elive init

clean:
	rm -rf *.beam erl_crash.dump
