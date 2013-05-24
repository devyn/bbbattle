#ifndef FMT_BBBATTLE_H
#define FMT_BBBATTLE_H

#include "misc.h"

#define READ_BBBATTLE_SUCCESS 0
#define READ_BBBATTLE_PREMATURE_EOF 1
#define READ_BBBATTLE_INTEGER_TOO_LONG 2
#define READ_BBBATTLE_EXPECTED_INTEGER 3
#define READ_BBBATTLE_INVALID_INPUT 4
#define READ_BBBATTLE_EXPECTED_NONZERO_INTEGER 5

int read_bbbattle(int *width, int *height, int *teams, char **alive, char **dying, struct rgb24 *team_colors, FILE *stream);

#endif
