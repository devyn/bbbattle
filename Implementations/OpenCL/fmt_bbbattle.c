#include <stdlib.h>
#include <stdio.h>
#include <limits.h>

#include "fmt_bbbattle.h"

enum parser_state {
  PARSE_WIDTH,
  PARSE_HEIGHT,
  PARSE_TEAMCOLOR_RED,
  PARSE_TEAMCOLOR_GREEN,
  PARSE_TEAMCOLOR_BLUE,
  PARSE_ALIVE,
  PARSE_DYING,
  PARSE_CELL_X,
  PARSE_CELL_Y,
  PARSE_END_TEAM
};

int read_bbbattle(int *width, int *height, int *teams, char **alive, char **dying, struct rgb24 *team_colors, FILE *stream) {
  enum   parser_state state = PARSE_WIDTH;
  enum   parser_state after;
  char   buf[1024];
  size_t len = 0;
  size_t i   = 0;

  char   scratch[128];
  size_t scratch_len = 0;

  char   team = 1;
  char  *cells;

  int    pt_x;
  int    pt_y;

  while (1) {
    if (i >= len) {
      if (feof(stream)) {
        if (state == PARSE_TEAMCOLOR_RED || state == PARSE_END_TEAM) {
          return READ_BBBATTLE_SUCCESS;
        } else {
          return READ_BBBATTLE_PREMATURE_EOF;
        }
      } else {
        len = fread(buf, 1, 1024, stream);
        i   = 0;
      }
    } else {
      switch (state) {
        case PARSE_WIDTH:
          if (buf[i] >= '0' && buf[i] <= '9') {
            if (scratch_len >= 128) {
              return READ_BBBATTLE_INTEGER_TOO_LONG;
            } else {
              scratch[scratch_len++] = buf[i++];
            }
          } else if (buf[i] == ' ') {
            if (scratch_len != 0) {
              scratch[scratch_len] = '\0';
              *width = strtoll(scratch, NULL, 10);

              if (*width == 0) {
                return READ_BBBATTLE_EXPECTED_NONZERO_INTEGER;
              }

              i++;
              scratch_len = 0;
              state = PARSE_HEIGHT;
            } else {
              i++;
            }
          } else if (buf[i] == '\n' && scratch_len == 0) {
            i++;
          } else {
            return READ_BBBATTLE_INVALID_INPUT;
          }
          break;
        case PARSE_HEIGHT:
          if (buf[i] >= '0' && buf[i] <= '9') {
            if (scratch_len >= 128) {
              return READ_BBBATTLE_INTEGER_TOO_LONG;
            } else {
              scratch[scratch_len++] = buf[i++];
            }
          } else if (buf[i] == '\n') {
            if (scratch_len != 0) {
              scratch[scratch_len] = '\0';
              *height = strtoll(scratch, NULL, 10);

              if (*height == 0) {
                return READ_BBBATTLE_EXPECTED_NONZERO_INTEGER;
              }

              i++;
              scratch_len = 0;
              state = PARSE_TEAMCOLOR_RED;

              *alive = calloc(1, (*width) * (*height) * sizeof(char));
              *dying = calloc(1, (*width) * (*height) * sizeof(char));
            } else {
              return READ_BBBATTLE_EXPECTED_INTEGER;
            }
          } else if (buf[i] == ' ' && scratch_len == 0) {
            i++;
          } else {
            return READ_BBBATTLE_INVALID_INPUT;
          }
          break;
        case PARSE_TEAMCOLOR_RED:
          if (buf[i] >= '0' && buf[i] <= '9') {
            if (scratch_len >= 128) {
              return READ_BBBATTLE_INTEGER_TOO_LONG;
            } else {
              scratch[scratch_len++] = buf[i++];
            }
          } else if (buf[i] == ' ') {
            if (scratch_len != 0) {
              scratch[scratch_len] = '\0';
              team_colors[team].red = strtoll(scratch, NULL, 10);

              i++;
              scratch_len = 0;
              state = PARSE_TEAMCOLOR_GREEN;
            } else {
              i++;
            }
          } else if (buf[i] == '\n' && scratch_len == 0) {
            i++;
          } else {
            return READ_BBBATTLE_INVALID_INPUT;
          }
          break;
        case PARSE_TEAMCOLOR_GREEN:
          if (buf[i] >= '0' && buf[i] <= '9') {
            if (scratch_len >= 128) {
              return READ_BBBATTLE_INTEGER_TOO_LONG;
            } else {
              scratch[scratch_len++] = buf[i++];
            }
          } else if (buf[i] == ' ') {
            if (scratch_len != 0) {
              scratch[scratch_len] = '\0';
              team_colors[team].green = strtoll(scratch, NULL, 10);

              i++;
              scratch_len = 0;
              state = PARSE_TEAMCOLOR_BLUE;
            } else {
              i++;
            }
          } else {
            return READ_BBBATTLE_INVALID_INPUT;
          }
          break;
        case PARSE_TEAMCOLOR_BLUE:
          if (buf[i] >= '0' && buf[i] <= '9') {
            if (scratch_len >= 128) {
              return READ_BBBATTLE_INTEGER_TOO_LONG;
            } else {
              scratch[scratch_len++] = buf[i++];
            }
          } else if (buf[i] == ':') {
            if (scratch_len != 0) {
              scratch[scratch_len] = '\0';
              team_colors[team].blue = strtoll(scratch, NULL, 10);

              i++;
              scratch_len = 0;
              state = PARSE_ALIVE;

              (*teams)++;
            } else {
              return READ_BBBATTLE_INVALID_INPUT;
            }
          } else if (buf[i] == ' ') {
            i++;
          } else {
            return READ_BBBATTLE_INVALID_INPUT;
          }
          break;
        case PARSE_ALIVE:
          if (buf[i] == 'a') {
            cells = *alive;
            i++;
            while (buf[i] == ' ' || buf[i] == '\n') i++;

            if (buf[i] == '.') {
              state = PARSE_DYING;
              i++;
            } else {
              state = PARSE_CELL_X;
              after = PARSE_DYING;
            }
          } else if (buf[i] == ' ' || buf[i] == '\n') {
            i++;
          } else {
            return READ_BBBATTLE_INVALID_INPUT;
          }
          break;
        case PARSE_DYING:
          if (buf[i] == 'd') {
            cells = *dying;
            i++;
            while (buf[i] == ' ' || buf[i] == '\n') i++;

            if (buf[i] == '.') {
              state = PARSE_END_TEAM;
              i++;
            } else {
              state = PARSE_CELL_X;
              after = PARSE_END_TEAM;
            }
          } else if (buf[i] == ' ' || buf[i] == '\n') {
            i++;
          } else {
            return READ_BBBATTLE_INVALID_INPUT;
          }
          break;
        case PARSE_CELL_X:
          if (buf[i] >= '0' && buf[i] <= '9') {
            if (scratch_len >= 128) {
              return READ_BBBATTLE_INTEGER_TOO_LONG;
            } else {
              scratch[scratch_len++] = buf[i++];
            }
          } else if (buf[i] == ',') {
            if (scratch_len != 0) {
              scratch[scratch_len] = '\0';
              pt_x = strtoll(scratch, NULL, 10);

              i++;
              scratch_len = 0;
              state = PARSE_CELL_Y;
            } else {
              return READ_BBBATTLE_INVALID_INPUT;
            }
          } else if (buf[i] == ' ' && scratch_len == 0) {
            i++;
          } else {
            return READ_BBBATTLE_INVALID_INPUT;
          }
          break;
        case PARSE_CELL_Y:
          if (buf[i] >= '0' && buf[i] <= '9') {
            if (scratch_len >= 128) {
              return READ_BBBATTLE_INTEGER_TOO_LONG;
            } else {
              scratch[scratch_len++] = buf[i++];
            }
          } else if (buf[i] == ' ' || buf[i] == '.') {
            if (scratch_len != 0) {
              scratch[scratch_len] = '\0';
              pt_y = strtoll(scratch, NULL, 10);

              scratch_len = 0;
              state = buf[i++] == '.' ? after : PARSE_CELL_X;

              cells[pt_y * (*width) + pt_x] = team;
            } else {
              return READ_BBBATTLE_INVALID_INPUT;
            }
          } else {
            return READ_BBBATTLE_INVALID_INPUT;
          }
          break;
        case PARSE_END_TEAM:
          if (buf[i] == '\n') {
            state = PARSE_TEAMCOLOR_RED;
            team++;
            i++;
          } else if (buf[i] == ' ') {
            i++;
          } else {
            return READ_BBBATTLE_INVALID_INPUT;
          }
          break;
      }
    }
  }
}
