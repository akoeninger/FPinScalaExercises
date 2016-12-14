#!/bin/bash
#
# A more capable sbt runner, coincidentally also called sbt.
# Author: Paul Phillips <paulp@improving.org>

java -Xmx1024M -Xss8m -XX:PermSize=300m -jar `dirname $0`/sbt-launch.jar "$@"
