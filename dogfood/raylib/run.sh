#/bin/zsh
set -e
set -x

/Users/knix/dev/k1/target/release/k1 \
--write-llvm -c -framework -c Cocoa -c -framework -c OpenGL -c -framework -c IOKit -c -framework -c CoreAudio -c -framework -c CoreVideo -c -framework -c AudioToolbox r .

