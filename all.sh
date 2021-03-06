#!/bin/sh

KERL_INSTALL_PATH=~/erlang
KERL_RELEASES="17.0 17.1.2 17.3"

make build-ct-suites

for rel in $KERL_RELEASES
do
	echo
	echo "    TESTING $rel"
	echo
	. $KERL_INSTALL_PATH/$rel/activate
	cp ~/.kerl/builds/$rel/otp_src_*/lib/ssl/test/erl_make_certs.erl \
		deps/ct_helper/src/
	CT_OPTS="-label $rel" make tests
done

xdg-open logs/all_runs.html
