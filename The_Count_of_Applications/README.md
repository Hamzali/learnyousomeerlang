## starting from erlang shell
- build both ppool and erlcount applications with `erl -make`
- start the shell with `erl -env ERL_LIBS "."`
- start both applicaitons in order `> application:start(ppool), application:start(erlcount).`
