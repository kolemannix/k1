@z = unnamed_addr constant i64 1337

define i64 @main(i64 %x, i64 %y) {
entry:
  %b = alloca i64, align 8
  %b_init = add i64 %x, 1
  store i64 %b_init, i64* %b
  %foo = alloca i64, align 8
  %z_val = load i64, i64* @z
  %foo_init = add i64 3, %z_val
  store i64 %foo_init, i64* %foo
  %ret_val_lhs = load i64, i64* %foo
  %ret_val = add i64 %ret_val_lhs, %y
  ret i64 %ret_val
}

