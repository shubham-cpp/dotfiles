#!/usr/bin/env bash

# NOTE:
# This doesn't work as is on Windows. You'll need to create an equivalent `.bat` file instead
java_version=$(javac -version | cut -d' ' -f2 | cut -d\. -f1)
if [[ $java_version -lt 11 ]]; then
   notify-send "Java Version Error" "Please install 11 or above"
   exit 1
fi
# NOTE:
# If you're not using Linux you'll need to adjust the `-configuration` option
# to point to the `config_mac' or `config_win` folders depending on your system.
TO_JDT="$HOME/Downloads/GitClones/eclipse.jdt.ls/org.eclipse.jdt.ls.product/target/repository"
JAR="$TO_JDT/plugins/org.eclipse.equinox.launcher_*.jar"
GRADLE_HOME=$HOME/.cache/gradle java \
  -Declipse.application=org.eclipse.jdt.ls.core.id1 \
  -Dosgi.bundles.defaultStartLevel=4 \
  -Declipse.product=org.eclipse.jdt.ls.core.product \
  -Dlog.protocol=true \
  -Dlog.level=ALL \
  -Xms1g \
  -Xmx2G \
  -jar $(echo "$JAR") \
  -configuration "$TO_JDT/config_linux" \
  -data "${1:-$HOME/.cache/jdtls_workspace}" \
  --add-modules=ALL-SYSTEM \
  --add-opens java.base/java.util=ALL-UNNAMED \
  --add-opens java.base/java.lang=ALL-UNNAMED
