#!/bin/bash
set -eu

castle-engine package --os=win64 --cpu=x86_64 --mode=release
castle-engine package --os=linux --cpu=x86_64 --mode=release
