#!/bin/bash
cd "$( dirname "${BASH_SOURCE[0]}" )/bin"
R --slave --vanilla --file=launch_shiny.r
