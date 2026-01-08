 #!/bin/sh

erl -pa $PWD/ebin/ -pa $PWD/deps/*/ebin/ -s daymate -sname daymate -config config/env/config
