mkdir -p ~/.k1/bin
rm ~/.k1/bin/k1lsp
cp k1lsp ~/.k1/bin/k1lsp
cp k1 ~/.k1/bin/k1
rm -r ~/.k1/k1lib
cp -r k1lib/. ~/.k1/k1lib 
echo "Compiling k1 runtime"
make -C ~/.k1/k1lib/core/libs
echo "K1 installed at ~/.k1"
echo "You should add ~/.k1/bin to your \$PATH"
