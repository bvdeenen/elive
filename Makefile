.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

.yrl.erl:
	erlc -W $<

ERL = erl -boot start_clean
MODS = elive

all: compile

compile: ${MODS:%=%.beam}

world: compile
	${ERL} -pa ~/elive/ -s worldrunner main 2

clean:
	rm -rf *.beam erl_crash.dump
