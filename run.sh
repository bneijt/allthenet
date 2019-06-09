#!/bin/bash
set -e
stack build
stack exec allthenet-exe -- --scan --draw
stack exec allthenet-exe -- --zoom
