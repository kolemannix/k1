#include <stdio.h>
struct String { long len; char* data; };
enum Tag { CommandUci, CommandIsReady, CommandPosition, CommandGo };
struct List { enum Tag tag; char bytes[128]; };
struct Position { enum Tag tag; struct String string; };
struct Uci { enum Tag tag; };
union Command {
  // Biggest variant does NOT have the biggest alignment!
  struct List as_list;
  struct Position as_position;
  struct Uci as_uci;
};

void asdf(union Command f) {
  union Command x;
  x.as_position.tag = CommandPosition;
  x.as_position.string.len = 4;
  printf("Command %lu", sizeof(union Command));
  printf("List %lu", sizeof(struct List));
  printf("Position %lu", sizeof(struct Position));
  printf("Uci %lu", sizeof(struct Uci));
  printf("String %lu", sizeof(struct String));
  x.as_list.tag = CommandGo;
  x.as_list.bytes[64] = 4;
  return;
}
