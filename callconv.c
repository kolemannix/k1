typedef struct { int i; } Small;
typedef struct { char i[5]; char x; } Small2;
typedef struct { char i[32]; char x; } Big1;
typedef struct { int i; int j; float k; } ClassesEz;
typedef struct { int i; int j; char k; } ClassesEz2;
typedef struct { char i[7]; short j; float k; } Classes;
typedef struct { int a; int b; union { double x; double y; int z; } c; } Union1;
typedef struct { long *a; long *b; } DoublePtr;

void takes_small(Small2 foo1) {
  return;
}
void call_small() {
  Small2 s = {0};
  takes_small(s);
}
void eb_pair_mixed(ClassesEz c) {
  c.i = 3;
  c.j = 4;
  return;
}
void call_eb_pair_mixed() {
  ClassesEz c = {0};
  eb_pair_mixed(c);
  return;
}

ClassesEz return_ebpair_mixed() {
  ClassesEz c = {0};
  return c;
}

void take_big1(Big1 big1) {
  big1.i[3] = 77;
  return;
}

void take_ptrs(DoublePtr dp) {
  return;
}
