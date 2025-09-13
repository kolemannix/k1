typedef struct Color {
  char r; char g; char b; char a;
} Color;

Color clear_red(Color color) {
  color.r = 0;
  return color;
}

int main(void) {
  return 0;
}
