#pragma once

#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>
#include <cstdlib>
#include <algorithm>
#include <cstdio>
#include "types.h"

#undef EOF

enum ETok: u8 {
  NUMBER,
  PLUS,
  FCALL,
  EOF
};
const char *ptok(ETok tok);

struct Token {
  char *tok;
  u16 length = 0;
  ETok etok;
  void print();
};
static_assert(sizeof(Token) <= 16, "Token too big");

class Parser {
  char *buf;
  u16 cur;
  u16 length;
public:
  explicit Parser(const char* file_name);
  ~Parser();
  Token next_tok();
  void skip_whitespace();
};