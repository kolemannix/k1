struct empty {};

struct foo { int i; struct empty e; struct { float f; } sf; };

struct empty take_empty(struct empty e)
{
    struct empty v;
    return v;
}

void take_foo(struct foo f)
{
  return;
}
