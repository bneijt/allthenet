#!/bin/bash
set -e
stack build
stack exec allthenet-exe -- --scan --draw
