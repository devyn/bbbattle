#ifndef FMT_BBBOUT_H
#define FMT_BBBOUT_H

#include <stdint.h>

#include "misc.h"

typedef struct {
  uint16_t width;
  uint16_t height;
  unsigned char teams;
  FILE *out;
} bbbout_stream;

typedef struct bbbout_cellset_row {
  uint16_t  y;
  uint16_t  size;
  uint16_t  capacity;
  uint16_t *points;

  uint16_t  height;

  struct bbbout_cellset_row *next;
} bbbout_cellset_row;

typedef struct {
  bbbout_cellset_row *first;
  bbbout_cellset_row *last;
} bbbout_cellset;

bbbout_stream *bbbout_open(char *path, uint16_t width, uint16_t height, unsigned char teams, struct rgb24 *team_colors);

void bbbout_close(bbbout_stream *stream);

int bbbout_write_generation(bbbout_stream *stream, uint32_t gen_id, char *alive, char *dying);

int bbbout_build_cellsets(bbbout_stream *stream, bbbout_cellset *teams, char *cells);

void bbbout_free_cellsets(bbbout_cellset *cellsets, int size);

int bbbout_write_cellset(bbbout_stream *stream, const bbbout_cellset cellset);

#endif
