#!/bin/bash

if [ -z "$EMACS" ]; then
    EMACS=emacs
fi


$EMACS -batch -q -l el-spy.el -l test/el-spy-test.el -f ert-run-tests-batch-and-exit


