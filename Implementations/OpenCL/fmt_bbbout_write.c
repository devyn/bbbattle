#include "fmt_bbbout_write.h"

int little_endian;

int i16tobe16(uint16_t i) {
  if (little_endian) {
    return (i << 8) | (i >> 8);
  } else {
    return i;
  }
}

int fput_be16(uint16_t i, FILE *out) {
  if (little_endian) {
    i = (i << 8) | (i >> 8);
  }
  return fwrite(&i, sizeof(uint16_t), 1, out);
}

int fput_be32(uint32_t i, FILE *out) {
  if (little_endian) {
    i = ((i << 8) & 0xFF00FF00) | ((i >> 8) & 0x00FF00FF);
    i = (i << 16) | (i >> 16);
  }
  return fwrite(&i, sizeof(uint32_t), 1, out);
}

bbbout_stream *bbbout_open_write(char *path, uint16_t width, uint16_t height, unsigned char teams, struct rgb24 *team_colors) {
  bbbout_stream *stream = malloc(sizeof(bbbout_stream));

  if (stream == NULL) {
    return NULL;
  }

  stream->mode   = BBBOUT_MODE_WRITE;
  stream->width  = width;
  stream->height = height;
  stream->teams  = teams;
  stream->file   = fopen(path, "wb");

  if (stream->file == NULL) {
    free(stream);
    return NULL;
  }

  /* begin writing header */

  fputs("bbbout1:", stream->file);

  /* check endianness */

  int n = 1;
  little_endian = ((char*) &n)[0];

  /* dimensions */

  fput_be16(width,  stream->file);
  fput_be16(height, stream->file);

  /* teams */

  fputc('T', stream->file);

  fput_be16(teams, stream->file);

  int i;
  for (i = 1; i <= teams; i++) {
    fput_be16(i, stream->file);
    fwrite(&team_colors[i], sizeof(struct rgb24), 1, stream->file);
  }

  return stream;
}

int bbbout_write_generation(bbbout_stream *stream, uint32_t gen_id, char *alive, char *dying, int *team_counts) {
  fputc('g', stream->file);

  fput_be32(gen_id, stream->file);

  char t;
  int x, y;

  bbbout_cellset *alive_cellsets = calloc(256, sizeof(bbbout_cellset));
  bbbout_build_cellsets(stream, alive_cellsets, alive, team_counts);

  bbbout_cellset *dying_cellsets;
  if (dying != NULL) {
    dying_cellsets = calloc(256, sizeof(bbbout_cellset));
    bbbout_build_cellsets(stream, dying_cellsets, dying, NULL);
  }

  for (t = 1; t <= stream->teams; t++) {
    if (alive_cellsets[t].first != NULL || (dying != NULL && dying_cellsets[t].first != NULL)) {
      fputc('t', stream->file);

      fput_be16(t, stream->file);

      fputc('a', stream->file);

      bbbout_write_cellset(stream, alive_cellsets[t]);

      if (dying != NULL) {
        fputc('d', stream->file);

        bbbout_write_cellset(stream, dying_cellsets[t]);
      }
    }
  }

  if (alive_cellsets[255].first != NULL || (dying != NULL && dying_cellsets[255].first != NULL)) {
    fputs("t\xFF\xFF", stream->file);

    fputc('a', stream->file);

    bbbout_write_cellset(stream, alive_cellsets[255]);

    if (dying != NULL) {
      fputc('d', stream->file);

      bbbout_write_cellset(stream, dying_cellsets[255]);
    }
  }

  bbbout_free_cellsets(&alive_cellsets[1],   stream->teams);
  bbbout_free_cellsets(&alive_cellsets[255], 1);

  if (dying != NULL) {
    bbbout_free_cellsets(&dying_cellsets[1],   stream->teams);
    bbbout_free_cellsets(&dying_cellsets[255], 1);
  }

  return 0;
}

int bbbout_build_cellsets(bbbout_stream *stream, bbbout_cellset *teams, char *cells, int *team_counts) {
  uint16_t x, y;
  size_t pt;

  for (x = 1; x <= stream->teams; x++) {
    teams[x].first = NULL;
    teams[x].last  = NULL;

    if (team_counts != NULL) team_counts[x] = 0;
  }

  teams[255].first = NULL;
  teams[255].last  = NULL;

  if (team_counts != NULL) team_counts[255] = 0;

  for (y = 0, pt = 0; y < stream->height; y++) {
    for (x = 0; x < stream->width; x++, pt++) {
      if (cells[pt] != 0) {

        if (team_counts != NULL) team_counts[(unsigned char) cells[pt]]++;

        bbbout_cellset_row *row = teams[(unsigned char) cells[pt]].last;

        if (row == NULL) {
          row = teams[(unsigned char) cells[pt]].first = teams[(unsigned char) cells[pt]].last = malloc(sizeof(bbbout_cellset_row));

          if (row == NULL) {
            bbbout_free_cellsets(&teams[1], stream->teams);
            bbbout_free_cellsets(&teams[255], 1);
            return -8;
          }

          row->y = y;
          row->capacity = 64;
          row->size = 0;
          row->points = malloc(sizeof(uint16_t) * row->capacity);
          row->height = 0;
          row->next = NULL;
        } else if (row->y != y) {
          row = teams[(unsigned char) cells[pt]].last->next = malloc(sizeof(bbbout_cellset_row));

          if (row == NULL) {
            bbbout_free_cellsets(&teams[1], stream->teams);
            bbbout_free_cellsets(&teams[255], 1);
            return -8;
          }

          row->y = y;
          row->capacity = 64;
          row->size = 0;
          row->points = malloc(sizeof(uint16_t) * row->capacity);
          row->height = teams[(unsigned char) cells[pt]].last->height + 1;
          row->next = NULL;

          teams[(unsigned char) cells[pt]].last = row;
        }

        if (row->size == row->capacity) {
          row->capacity *= 2;
          row->points = realloc(row->points, sizeof(uint16_t) * row->capacity);
        }

        row->points[row->size++] = i16tobe16(x);
      }
    }
  }

  return 0;
}

void bbbout_free_cellsets(bbbout_cellset *cellsets, int size) {
  int i;
  bbbout_cellset_row *cur_row, *to_be_freed;

  for (i = 0; i < size; i++) {
    cur_row = cellsets[i].first;

    while (cur_row != NULL) {
      to_be_freed = cur_row;
      cur_row = cur_row->next;

      free(to_be_freed->points);
      free(to_be_freed);
    }

    cellsets[i].first = 0;
    cellsets[i].last  = 0;
  }
}

int bbbout_write_cellset(bbbout_stream *stream, const bbbout_cellset cellset) {
  if (cellset.first == NULL) {
    fput_be16(0, stream->file);
  } else {
    bbbout_cellset_row *row = cellset.first;

    fput_be16(cellset.last->height + 1, stream->file);

    while (row != NULL) {
      fput_be16(row->y,    stream->file);
      fput_be16(row->size, stream->file);
      fwrite(row->points, sizeof(uint16_t), row->size, stream->file);
      row = row->next;
    }
  }

  return 0;
}
