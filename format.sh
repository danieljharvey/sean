#!/bin/bash

echo "Formatting..."

find "./src" -type f -iname *.purs -exec purty --write --dynamic '{}' \;

find "./test" -type f -iname *.purs -exec purty --write --dynamic '{}' \;

echo "Done!"
