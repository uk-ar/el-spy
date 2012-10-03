#!/bin/bash

if [ -z "$EMACS" ]; then
    $EMACS=emacs
fi


$EMACS -batch -q -l test-double.el -l test/test-double-test.el -f ert-run-tests-batch-and-exit


