#include <raylib.h>
#include <rlgl.h>

#define RAYGUI_IMPLEMENTATION

#include <raygui.h>

#include <GL/glew.h>
#include "stb_image.h"

#include "parser.h"

const static Vector2 ZERO = Vector2(0.0, 0.0);

void screen_print(const stbi_uc *font, Color *buf, u16 x, u16 y, const char *str, u32 len) {
  for (u32 i = 0; i < len; i++, x += 6) {
    char c = str[i] - ' ';
    u16 fy = (c / 10) * 9;
    u16 fx = (c % 10) * 6;
    for (u16 sx = 0; sx < 6; sx++) {
      for (u16 sy = 0; sy < 9; sy++) {
        buf[(y + sy) * 100 + (x + sx)] = font[(sy + fy) * 60 + (sx + fx)] ? WHITE : BLACK;
      }
    }
  }
}

i32 main() {
  InitWindow(1000, 1000, "HaQ");

  auto image = GenImageColor(100, 100, RED);
  UnloadImage(image);
  auto texture = LoadTextureFromImage(image);
  auto buffer = (Color *) calloc(100, 100 * 4);

  i32 size_x, size_y, chan;
  auto font_png = stbi_load("font.png", &size_x, &size_y, &chan, 1);

  Parser parser("hello.haq");

  Token tok;
  do {
    tok = parser.next_tok();
    tok.print();
  } while (tok.etok != EOF);

  screen_print(font_png, (Color *) buffer, 0, 0, "print", 5);

  while (!WindowShouldClose()) {
    glBindTexture(GL_TEXTURE_2D, texture.id);
    glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, 100, 100, GL_RGBA, GL_UNSIGNED_BYTE, buffer);
    glBindTexture(GL_TEXTURE_2D, 0);
    BeginDrawing();
    DrawTexturePro(texture, Rectangle(0.0, 0.0, 100.0, 100.0), Rectangle(0.0, 0.0, 1000.0, 1000.0), ZERO, 0.0, WHITE);
    EndDrawing();
  }

  UnloadTexture(texture);
  CloseWindow();
  stbi_image_free(font_png);
  free(buffer);
  return 0;
}