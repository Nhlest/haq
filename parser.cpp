#include "parser.h"

Parser::Parser(const char *file_name) {
  auto fd = open(file_name, O_RDONLY);
  struct stat f_stat{};
  stat(file_name, &f_stat);
  length = f_stat.st_size;
  buf = (char*)malloc(length);
  read(fd, buf, length);
  cur = 0;
}

Parser::~Parser() {
  free(buf);
}

Token Parser::next_tok() {
  if (cur >= length) {
    return Token { .etok = EOF };
  }
  u16 l = 0;
  for (;buf[cur+l] != ' ' && cur + l < length;l++);
  Token tok;
  if (std::all_of(buf+cur, buf+cur+l, [](char i){ return i >= '0' && i <= '9'; })) {
    tok = Token { .etok = NUMBER, .tok = buf+cur, .length = l };
  } else if (buf[cur] == '+' && l == 1) {
    tok = Token { .etok = PLUS };
  } else {
    tok = Token { .etok = FCALL, .tok = buf+cur, .length = l };
  }
  cur += l;
  skip_whitespace();
  return tok;
}

void Parser::skip_whitespace() {
  for (;buf[cur] == ' ' && cur < length;cur++) {}
}

const char *ptok(ETok tok) {
  switch(tok) {
    case NUMBER:
      return "NUMBER";
    case PLUS:
      return "PLUS";
    case FCALL:
      return "FCALL";
    case EOF:
      return "EOF";
  }
}

void Token::print() {
  printf("%s %.*s\n", ptok(etok), length, tok);
}