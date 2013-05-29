#include "fmt_bbbout_read.h"

int fget_be16(uint16_t *dest, FILE *src) {
  if (fread(dest, sizeof(uint16_t), 1, src) == 1) {

#ifndef INTEGERS_ARE_BIG_ENDIAN
    *dest = ((*dest) >> 8) | ((*dest) << 8);
#endif

    return 1;
  } else {
    return 0;
  }
}

int fget_be16x4(uint64_t *dest, FILE *src) {
  if (fread(dest, sizeof(uint64_t), 1, src) == 1) {

#ifndef INTEGERS_ARE_BIG_ENDIAN
      *dest = (((*dest) & 0xFF00FF00FF00FF00) >> 8) | (((*dest) & 0x00FF00FF00FF00FF) << 8);
#endif

    return 4;
  } else {
    return 0;
  }
}

int fget_be32(uint32_t *dest, FILE *src) {
  if (fread(dest, sizeof(uint32_t), 1, src) == 1) {

#ifndef INTEGERS_ARE_BIG_ENDIAN
      *dest = (((*dest) << 8) & 0xFF00FF00) | (((*dest) >> 8) & 0x00FF00FF);
      *dest = ((*dest) << 16) | ((*dest) >> 16);
#endif

    return 1;
  } else {
    return 0;
  }
}

bbbout_stream *bbbout_open_read(char *path, int *err) {

  bbbout_stream *stream = malloc(sizeof(bbbout_stream));

  if (stream == NULL) {
    *err = BBBOUT_MEMORY_ALLOCATION_ERROR;

    return NULL;
  }

  FILE *file = fopen(path, "rb");

  if (file == NULL) {
    *err = BBBOUT_FILE_OPEN_ERROR;

    free(stream);
    return NULL;
  }

  stream->mode = BBBOUT_MODE_READ;
  stream->file = file;

  char buf[256];

#define close_and_err(err_code) *err = (err_code); fclose(file); free(stream); return NULL;

  /* check magic string */

  fread(buf, sizeof(char), 8, file);

  if (memcmp(buf, "bbbout1:", 8) != 0) {
    close_and_err(BBBOUT_HEADER_INVALID_ERROR);
  }

  /* read width and height and verify */

  if (!(fget_be16(&stream->width, file) && fget_be16(&stream->height, file))) {
    close_and_err(BBBOUT_READ_ERROR);
  }

  if (stream->width == 0 || stream->height == 0) {
    close_and_err(BBBOUT_HEADER_INVALID_ERROR);
  }

  /* read number of teams and verify */

  if (fgetc(file) != 'T') {
    close_and_err(BBBOUT_HEADER_INVALID_ERROR);
  }

  uint16_t teams = 0;

  if (!fget_be16(&teams, file)) {
    close_and_err(BBBOUT_READ_ERROR);
  }

  if (teams > 254) {
    close_and_err(BBBOUT_UNSUPPORTED_VALUE_ERROR);
  }

  stream->teams = teams;

  /* read teams and colours.
   * FIXME: team index 0 not supported
   */

  while (teams--) {
    uint16_t team;

    fget_be16(&team, file);

    if ((team == 0) || (team > 254)) {
      close_and_err(BBBOUT_UNSUPPORTED_VALUE_ERROR);
    }

    stream->team_colors[team].red   = fgetc(file);
    stream->team_colors[team].green = fgetc(file);
    stream->team_colors[team].blue  = fgetc(file);
  }

  /* also add team colour 0 and 255 */

  stream->team_colors[0].red   = 0;
  stream->team_colors[0].green = 0;
  stream->team_colors[0].blue  = 0;

  stream->team_colors[255].red   = 255;
  stream->team_colors[255].green = 255;
  stream->team_colors[255].blue  = 255;

  *err = BBBOUT_SUCCESS;
  return stream;
}

int bbbout_read_generation(bbbout_stream *stream, uint32_t *gen_id, char *alive, char *dying) {
  int err;

  if (feof(stream->file)) {
    return BBBOUT_READ_ERROR;
  }

  /* initialize memory */

  size_t s;
  size_t mem_size = stream->width * stream->height;

  for (s = 0; s < mem_size; s++) {
    alive[s] = 0;
  }

  if (dying != NULL) {
    for (s = 0; s < mem_size; s++) {
      dying[s] = 0;
    }
  }

  /* read generation ID */

  if (fgetc(stream->file) != 'g') {
      printf("ln %i seek %li\n", __LINE__, ftell(stream->file));
    return BBBOUT_GENERATION_INVALID_ERROR;
  }

  fget_be32(gen_id, stream->file);

  /* read team generations */

  while (1) {
    if (feof(stream->file)) {
      return BBBOUT_SUCCESS;
    }

    if (fgetc(stream->file) != 't') {
      return BBBOUT_TEAM_GENERATION_INVALID_ERROR;
    }

    uint16_t team_16;
    char     team;

    fget_be16(&team_16, stream->file);

    if (team_16 == 65535) {
      team = -1;
    } else if (team_16 == 0 || team_16 > 254) {
      return BBBOUT_UNSUPPORTED_VALUE_ERROR;
    } else {
      team = team_16;
    }

    if (fgetc(stream->file) != 'a') {
      return BBBOUT_TEAM_GENERATION_INVALID_ERROR;
    }

    err = bbbout_read_cellset(stream, alive, team);
    if (err != BBBOUT_SUCCESS) {
      return err;
    }

    if (feof(stream->file)) {
      return BBBOUT_SUCCESS;
    }

    char next = fgetc(stream->file);

    switch (next) {
      case 'd':
        if (dying == NULL) {
          err = bbbout_skip_cellset(stream);
        } else {
          err = bbbout_read_cellset(stream, dying, team);
        }

        if (err != BBBOUT_SUCCESS) {
          return err;
        }

        next = fgetc(stream->file);

        switch (next) {
          case 't':
            ungetc('t', stream->file);
            break;
          case 'g':
            ungetc('g', stream->file);
            return BBBOUT_SUCCESS;
          default:
            return BBBOUT_TEAM_GENERATION_INVALID_ERROR;
        }
        break;
      case 't':
        ungetc('t', stream->file);
        break;
      case 'g':
        ungetc('g', stream->file);
        return BBBOUT_SUCCESS;
      default:
        return BBBOUT_TEAM_GENERATION_INVALID_ERROR;
    }
  }
}

int bbbout_read_cellset(bbbout_stream *stream, char *cells, char team) {
  uint16_t rows = 0;

  fget_be16(&rows, stream->file);

  while (rows--) {
    uint16_t y = 0, entries = 0;

    fget_be16(&y,       stream->file);
    fget_be16(&entries, stream->file);

    while (entries > 0) {
      uint16_t x4[4];

      if (entries >= 4) {
        fget_be16x4((uint64_t *) x4, stream->file);

        cells[y * stream->width + x4[0]] = team;
        cells[y * stream->width + x4[1]] = team;
        cells[y * stream->width + x4[2]] = team;
        cells[y * stream->width + x4[3]] = team;

        entries -= 4;
      } else {
        fget_be16(x4, stream->file);

        cells[y * stream->width + x4[0]] = team;

        entries--;
      }
    }
  }

  return BBBOUT_SUCCESS;
}

int bbbout_skip_cellset(bbbout_stream *stream) {
  uint16_t rows = 0;

  fget_be16(&rows, stream->file);

  while (rows--) {
    uint16_t entries = 0;

    fseek(stream->file, sizeof(uint16_t), SEEK_CUR);

    fget_be16(&entries, stream->file);

    fseek(stream->file, entries * sizeof(uint16_t), SEEK_CUR);
  }

  return BBBOUT_SUCCESS;
}
