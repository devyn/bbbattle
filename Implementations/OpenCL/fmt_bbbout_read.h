#ifndef FMT_BBBOUT_READ_H
#define FMT_BBBOUT_READ_H

#include "fmt_bbbout_common.h"

#include <string.h>

bbbout_stream *bbbout_open_read(char *path, int *err);

int bbbout_read_generation(bbbout_stream *stream, uint32_t *gen_id, char *alive, char *dying);

int bbbout_read_cellset(bbbout_stream *stream, char *cells, char team);

int bbbout_skip_cellset(bbbout_stream *stream);

#endif
