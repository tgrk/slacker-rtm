#!/bin/bash

erl -pa deps/*/ebin ebin \
    -boot start_sasl \
    -sname sr@$(hostname) \
    -noshell \
    -detached \
    -s sr
