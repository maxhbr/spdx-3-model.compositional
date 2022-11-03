#!/usr/bin/env bash

cd "$(dirname "$0")"
echo model.puml | entr -r plantuml -png /_
