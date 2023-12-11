struct Foo {
  long size;
  void* data;
};

int main() {
  bool asdf = true;
    Foo foo = {asdf ? 1 : 2, 0};
  return 0;
}
