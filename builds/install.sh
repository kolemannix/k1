mkdir -p ~/.k1/bin
cp k1lsp ~/.k1/bin/k1lsp
cp k1 ~/.k1/bin/k1
cp -r k1lib ~/.k1/k1lib 
make -C ~/.k1/k1lib/core
echo "K1 installed at ~/.k1"
echo "You should add ~/.k1/bin to your \$PATH"
