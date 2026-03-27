mkdir ~/.k1
cp k1lsp ~/.k1/bin/k1lsp
cp k1 ~/.k1/bin/k1
cp -r k1lib ~/.k1/lib 
make -C ~/.k1/lib/core
echo "K1 installed at ~/.k1"
echo "You should add ~/.k1/bin to your \$PATH"


