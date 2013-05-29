#include "fmt_bbbout_write.h"

#ifdef INTEGERS_ARE_BIG_ENDIAN
#define i16tobe16(i) i
#else
#define i16tobe16(i) ((i) << 8) | ((i) >> 8)
#endif

int fput_be16(uint16_t i, FILE *out) {

#ifndef INTEGERS_ARE_BIG_ENDIAN
  i = (i << 8) | (i >> 8);
#endif

  return fwrite(&i, sizeof(uint16_t), 1, out);
}

int fput_be32(uint32_t i, FILE *out) {

#ifndef INTEGERS_ARE_BIG_ENDIAN
  i = ((i << 8) & 0xFF00FF00) | ((i >> 8) & 0x00FF00FF);
  i = (i << 16) | (i >> 16);
#endif

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
    teams[x].number_of_rows = 0;
    teams[x].first = NULL;
    teams[x].last  = NULL;

    if (team_counts != NULL) team_counts[x] = 0;
  }

  teams[255].number_of_rows = 0;
  teams[255].first = NULL;
  teams[255].last  = NULL;

  if (team_counts != NULL) team_counts[255] = 0;

  for (y = 0, pt = 0; y < stream->height; y++) {
    for (x = 0; x < stream->width; x++, pt++) {
      if (cells[pt] != 0) {

        if (team_counts != NULL) team_counts[(unsigned char) cells[pt]]++;

        bbbout_cellset_rowgroup *rowgroup = teams[(unsigned char) cells[pt]].last;

        bbbout_cellset_row *row;

        if (rowgroup == NULL) {
          rowgroup = teams[(unsigned char) cells[pt]].first = teams[(unsigned char) cells[pt]].last = malloc(sizeof(bbbout_cellset_rowgroup));

          if (rowgroup == NULL) {
            bbbout_free_cellsets(&teams[1], stream->teams);
            bbbout_free_cellsets(&teams[255], 1);
            return -8;
          }

          rowgroup->size = 1;
          rowgroup->next = NULL;

          row = &rowgroup->rows[0];

          row->y = y;
          row->size = 0;
          row->expansion = NULL;

          teams[(unsigned char) cells[pt]].number_of_rows++;
        } else {
          row = &rowgroup->rows[rowgroup->size - 1];
        }

        if (row->y != y) {
          if (rowgroup->size == BBBOUT_CELLSET_ROWGROUP_CAPACITY) {
            // allocate new rowgroup
            teams[(unsigned char) cells[pt]].last->next = rowgroup = malloc(sizeof(bbbout_cellset_rowgroup));
            teams[(unsigned char) cells[pt]].last = teams[(unsigned char) cells[pt]].last->next;

            if (rowgroup == NULL) {
              bbbout_free_cellsets(&teams[1], stream->teams);
              bbbout_free_cellsets(&teams[255], 1);
              return -8;
            }

            rowgroup->size = 1;
            rowgroup->next = NULL;

            row = &rowgroup->rows[0];
          } else {
            // add to rowgroup
            row = &rowgroup->rows[rowgroup->size++];
          }

          row->y = y;
          row->size = 0;
          row->expansion = NULL;

          teams[(unsigned char) cells[pt]].number_of_rows++;
        }

        if (row->size >= BBBOUT_CELLSET_ROW_CAPACITY) {
          if (row->expansion == NULL) {
            row->expansion = malloc(sizeof(bbbout_cellset_row_expansion));

            if (row->expansion == NULL) {
              bbbout_free_cellsets(&teams[1], stream->teams);
              bbbout_free_cellsets(&teams[255], 1);
              return -8;
            }

            row->expansion->expansion = NULL;
          } else if (row->size % BBBOUT_CELLSET_ROW_CAPACITY == 0) {
            row->expansion->expansion = malloc(sizeof(bbbout_cellset_row_expansion));

            if (row->expansion->expansion == NULL) {
              bbbout_free_cellsets(&teams[1], stream->teams);
              bbbout_free_cellsets(&teams[255], 1);
              return -8;
            }

            row->expansion = row->expansion->expansion;
            row->expansion->expansion = NULL;
          }

          row->expansion->points[row->size++ % BBBOUT_CELLSET_ROW_CAPACITY] = i16tobe16(x);
        } else {
          row->points[row->size++] = i16tobe16(x);
        }
      }
    }
  }

  return 0;
}

void bbbout_free_cellsets(bbbout_cellset *cellsets, int size) {
  int i, j;
  bbbout_cellset_rowgroup *cur_row_group, *to_be_freed_group;
  bbbout_cellset_row_expansion *cur_row_expansion, *to_be_freed_expansion;

  for (i = 0; i < size; i++) {
    cur_row_group = cellsets[i].first;

    while (cur_row_group != NULL) {
      to_be_freed_group = cur_row_group;
      cur_row_group = cur_row_group->next;

      for (j = 0; j < to_be_freed_group->size; j++) {
        cur_row_expansion = to_be_freed_group->rows[j].expansion;

        while (cur_row_expansion != NULL) {
          to_be_freed_expansion = cur_row_expansion;
          cur_row_expansion = cur_row_expansion->expansion;

          free(to_be_freed_expansion);
        }
      }

      free(to_be_freed_group);
    }

    cellsets[i].first = 0;
    cellsets[i].last  = 0;
  }
}

int bbbout_write_cellset(bbbout_stream *stream, const bbbout_cellset cellset) {
  if (cellset.first == NULL) {
    fput_be16(0, stream->file);
  } else {
    bbbout_cellset_rowgroup *rowgroup = cellset.first;

    fput_be16(cellset.number_of_rows, stream->file);

    while (rowgroup != NULL) {
      int i;

      for (i = 0; i < rowgroup->size; i++) {
        bbbout_cellset_row *row = &rowgroup->rows[i];

        bbbout_cellset_row_expansion *expansion = row->expansion;

        fput_be16(row->y,    stream->file);
        fput_be16(row->size, stream->file);

        if (expansion) {
          fwrite(row->points, sizeof(uint16_t), BBBOUT_CELLSET_ROW_CAPACITY, stream->file);
          fwrite(row->expansion->points, sizeof(uint16_t), row->size % BBBOUT_CELLSET_ROW_CAPACITY, stream->file);

          while ((expansion = expansion->expansion) != NULL) {
            fwrite(row->expansion->points, sizeof(uint16_t), BBBOUT_CELLSET_ROW_CAPACITY, stream->file);
          }
        } else {
          fwrite(row->points, sizeof(uint16_t), row->size, stream->file);
        }
      }

      rowgroup = rowgroup->next;
    }
  }

  return 0;
}
