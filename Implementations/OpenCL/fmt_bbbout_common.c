#include "fmt_bbbout_common.h"

void bbbout_close(bbbout_stream *stream) {
  fclose(stream->file);
  free(stream);
}

char *bbbout_strerror(int error_code) {
  switch (error_code) {
    case BBBOUT_END_OF_STREAM:
      return "end of stream (BUG: not an error!)";
    case BBBOUT_SUCCESS:
      return "success (BUG: not an error!)";
    case BBBOUT_MEMORY_ALLOCATION_ERROR:
      return "error while attempting to allocate memory";
    case BBBOUT_FILE_OPEN_ERROR:
      return "error while attempting to open file";
    case BBBOUT_HEADER_INVALID_ERROR:
      return "not a valid bbbout header";
    case BBBOUT_READ_ERROR:
      return "failed to read input: possibly EOF";
    case BBBOUT_UNSUPPORTED_VALUE_ERROR:
      return "one or more values in the bbbout file are not supported by this implementation";
    case BBBOUT_GENERATION_INVALID_ERROR:
      return "a generation structure is invalid";
    case BBBOUT_TEAM_GENERATION_INVALID_ERROR:
      return "a team generation structure is invalid";
    case BBBOUT_CELLSET_INVALID_ERROR:
      return "a cellset structure is invalid";
    default:
      return "";
  }
}
