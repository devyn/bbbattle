#ifndef FMT_BBBOUT_COMMON_H
#define FMT_BBBOUT_COMMON_H

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

#include "misc.h"

#define BBBOUT_MODE_READ 0
#define BBBOUT_MODE_WRITE 1

#define BBBOUT_SUCCESS 0
#define BBBOUT_MEMORY_ALLOCATION_ERROR -1
#define BBBOUT_FILE_OPEN_ERROR -2
#define BBBOUT_HEADER_INVALID_ERROR -3
#define BBBOUT_READ_ERROR -4
#define BBBOUT_UNSUPPORTED_VALUE_ERROR -5
#define BBBOUT_GENERATION_INVALID_ERROR -6
#define BBBOUT_TEAM_GENERATION_INVALID_ERROR -7
#define BBBOUT_CELLSET_INVALID_ERROR -8

typedef struct {
  int mode;
  uint16_t width;
  uint16_t height;
  unsigned char teams;

  struct rgb24 team_colors[256];

  FILE *file;
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

void bbbout_close(bbbout_stream *stream);

char *bbbout_strerror(int error_code);

#endif
