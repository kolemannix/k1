union Part {
  struct { long tag; char c; } AsChar;
  struct { long tag; long x; long y; } AsString;
};


int main() {
    // Access elements to prevent optimization
    union Part p;
    p.AsChar.tag = 0;

    if (p.AsChar.tag == 0) {
      return 1;
    } else {
      return 0;
    }
}
