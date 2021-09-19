#!/usr/bin/env bash

sed -e "/{{SOLUTION}}/r $2" $1 -e '/{{SOLUTION}}/d' > $3
