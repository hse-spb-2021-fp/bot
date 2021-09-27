#!/usr/bin/env bash

ulimit -v 524288 && ulimit -t 20 && "$1"
