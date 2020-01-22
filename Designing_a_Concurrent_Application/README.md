# Reminder app

- to build run `erl -make`
- to start run `erl -pa ebin` or run `erl` and in the erlang shell run `make:all([load].)`

# How to test it?

- after opening the erlang shell execute `evserv:start().`
- subscribe to listen for events in server `evserv:subscribe(self()).`
- add an event with timeout `evserv:add_event("EventName", "Description", {{2020, 1, 22}, {13, 56, 20}}).`
- listen for it's reminder message `evserv:listen(5).`
- cancel an event `evserv:cancel("EventName").`
