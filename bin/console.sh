#!/bin/bash

erl -remsh sr@$(hostname) -sname sr_$RANDOM@$(hostname)
