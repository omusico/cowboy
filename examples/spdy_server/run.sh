erlc spdy.erl && erl -pa ../../ebin/ -pa ../../deps/*/ebin  -boot start_sasl -eval 'spdy:start().'
