#!/usr/bin/env bash

convert -crop 2000x4000 +repage $1 ${1/.png/_%d.png}
