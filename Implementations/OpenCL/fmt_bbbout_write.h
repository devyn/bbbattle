#ifndef FMT_BBBOUT_WRITE_H
#define FMT_BBBOUT_WRITE_H

#include "fmt_bbbout_common.h"

bbbout_stream *bbbout_open_write(char *path, uint16_t width, uint16_t height, unsigned char teams, struct rgb24 *team_colors);

int bbbout_write_generation(bbbout_stream *stream, uint32_t gen_id, char *alive, char *dying, int *team_counts);

int bbbout_write_scanned_generation(bbbout_stream *stream, uint32_t gen_id, unsigned short *alive, int *team_counts);

int bbbout_build_cellsets(bbbout_stream *stream, bbbout_cellset *teams, char *cells, int *team_counts);

void bbbout_free_cellsets(bbbout_cellset *cellsets, int size);

int bbbout_write_cellset(bbbout_stream *stream, const bbbout_cellset cellset);

#endif
