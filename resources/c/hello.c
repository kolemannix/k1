typedef struct {
    long *x;
    long y;
    long z;
} Point;

Point make_point() {
  Point p;
  long x = 42;
  p.x = &x;
  p.y = 13;
  return p;
}

int main() {
  return 0;
}
