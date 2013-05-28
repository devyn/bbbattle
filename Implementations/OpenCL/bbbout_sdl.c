#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

#include <SDL/SDL.h>

#include "fmt_bbbout_read.h"

SDL_Surface *screen;

void handle_events() {
  SDL_Event event;

  while (SDL_PollEvent(&event)) {
    switch (event.type) {
      case SDL_QUIT:
        exit(0);
        break;
    }
  }
}

int main(int argc, char **argv) {
  if (argc < 2) {
    fprintf(stderr, "Usage: %s <bbbout-file>\n", argv[0]);
    return 1;
  }

  int err;
  bbbout_stream *bbbout = bbbout_open_read(argv[1], &err);

  if (err != BBBOUT_SUCCESS) {
    fprintf(stderr, "%s: while reading '%s': %s", argv[0], argv[1], bbbout_strerror(err));
    return 1;
  }

  const int width = bbbout->width, height = bbbout->height;

  if (SDL_Init(SDL_INIT_VIDEO | SDL_INIT_TIMER) < 0) {
    fprintf(stderr, "Error initializing SDL: %s\n", SDL_GetError());
    return 1;
  }

  screen = SDL_SetVideoMode(width, height, 32, SDL_HWSURFACE | SDL_DOUBLEBUF);
  if (screen == NULL) {
    fprintf(stderr, "Unable to set %ix%i at 32 bpp: %s\n", width, height, SDL_GetError());
  }

  uint32_t gen_id;

  char *alive = malloc(width * height * sizeof(char));
  char *dying = malloc(width * height * sizeof(char));

  err = bbbout_read_generation(bbbout, &gen_id, alive, dying);

  while (err == BBBOUT_SUCCESS) {
    unsigned int ticks1 = SDL_GetTicks();

    handle_events();

    int i;

    for (i = 0; i < width * height; i++) {
      if (dying[i]) {
        struct rgb24 team_color = bbbout->team_colors[(unsigned char) dying[i]];

        *((uint32_t *) screen->pixels + i) = SDL_MapRGB(screen->format, team_color.red/2, team_color.green/2, team_color.blue/2);
      } else {
        *((uint32_t *) screen->pixels + i) = 0;
      }
    }

    for (i = 0; i < width * height; i++) {
      if (alive[i]) {
        struct rgb24 team_color = bbbout->team_colors[(unsigned char) alive[i]];

        *((uint32_t *) screen->pixels + i) = SDL_MapRGB(screen->format, team_color.red, team_color.green, team_color.blue);
      }
    }

    SDL_Flip(screen);

    char *temp = dying;
    dying = alive;
    alive = temp;

    err = bbbout_read_generation(bbbout, &gen_id, alive, NULL);

    unsigned int ticks2 = SDL_GetTicks();

    if (ticks2 - ticks1 >= 33) {
      SDL_Delay(1);
    } else {
      SDL_Delay(33 - (ticks2 - ticks1));
    }
  }

  puts(bbbout_strerror(err));

  free(alive);
  free(dying);

  SDL_Quit();

  return 0;
}
