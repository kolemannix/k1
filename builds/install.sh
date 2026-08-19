set -e

echo "Installing k1 binaries in ~/.k1"
mkdir -p ~/.k1/bin

rm -f ~/.k1/bin/k1 ~/.k1/bin/k1lsp ~/.k1/bin/k1_test
cp k1 k1lsp k1_test ~/.k1/bin/

rm -rf ~/.k1/modules ~/.k1/test_src
cp -r modules/. ~/.k1/modules
cp -r test_src/. ~/.k1/test_src

echo "K1 installed at ~/.k1"
echo "You should add ~/.k1/bin to your \$PATH"
