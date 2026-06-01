echo "Installing k1 binaries in ~/.k1"
mkdir -p ~/.k1/bin

rm ~/.k1/bin/k1lsp
cp k1lsp ~/.k1/bin/k1lsp

rm ~/.k1/bin/k1
cp k1 ~/.k1/bin/k1

rm ~/.k1/bin/k1_test
cp k1_test ~/.k1/bin/k1_test

rm -r ~/.k1/k1lib
cp -r k1lib/. ~/.k1/k1lib 

rm -r ~/.k1/test_src
cp -r test_src/. ~/.k1/test_src 

echo "Compiling k1 runtime"
make -C ~/.k1/k1lib/core/libs

echo "K1 installed at ~/.k1"
echo "You should add ~/.k1/bin to your \$PATH"
