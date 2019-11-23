#!/bin/bash - 
#===============================================================================
#
#          FILE: launch.sh
# 
#         USAGE: ./launch.sh 
# 
#   DESCRIPTION: 
# 
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR:  JONATAN BORKOWSKI  
#       CREATED: 23/11/19 18:59
#      REVISION:  ---
#===============================================================================

set -o nounset                              # Treat unset variables as an error
# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# Launch bar1 and bar2
polybar -r topbar &

