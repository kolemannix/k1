long long myMul(long long x, long long y) {
  return x * y;
}

typedef struct Point2 { char x; char y; } Point2;

Point2 make(char x, char y) {
  Point2 p = {0};
  p.x = x;
  p.y = y;
  return p;
}

Point2 origin() {
  return make(0, 0);
}

