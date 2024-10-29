.global _main
.align 4

my_exit:
  mov x0, #0
  mov X16, #1
  svc #0x80

my_print:
  mov X0, #1
  adr X1, greeting
  mov X2, #13
  mov X16, #4
  svc #0x80
  ret

_main:
  bl my_print
  b my_exit


greeting: .ascii "Hello, world!\n"
