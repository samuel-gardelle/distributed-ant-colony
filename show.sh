#!/bin/sh

rm -rf graphs
mkdir -p graphs

stack run

find graphs -name *.gv | xargs -P 4 -i /usr/bin/dot {} -Tpng -O

#find graphs -name *.png | sed s%graphs/graph%% | sed s/.png// | sort -n | xargs -i echo graphs/graph{}.png | sxiv -i
