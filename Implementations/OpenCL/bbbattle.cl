#define check_neighbors(nx, ny) \
  if ((y + (ny) >= 0) & (y + (ny) < HEIGHT) & (x + (nx) >= 0) & (x + (nx) < WIDTH)) { \
    char cell = alive[(y + (ny)) * WIDTH + (x + (nx))]; \
\
    if (cell != 0) { \
      alive_neighbor_count++; \
\
      if (new_team != -1) { \
        if (new_team != 0) { \
          if (new_team != cell) { \
            new_team = -1; \
          } \
        } else { \
          new_team = cell; \
        } \
      } \
    } \
  }

__kernel void step_bbbattle(__global const char *alive, __global const char *dying, __global char *new_alive) {
  const int x = get_global_id(0);
  const int y = get_global_id(1);

  const size_t p = y*WIDTH + x;

  if (alive[p] != 0 || dying[p] != 0) {
    new_alive[p] = 0;
  } else {
    int  alive_neighbor_count = 0;
    char new_team = 0;

    check_neighbors(-1,-1);
    check_neighbors( 0,-1);
    check_neighbors( 1,-1);
    check_neighbors(-1, 0);
    check_neighbors( 1, 0);
    check_neighbors(-1, 1);
    check_neighbors( 0, 1);
    check_neighbors( 1, 1);

    if (alive_neighbor_count == 2) {
      new_alive[p] = new_team;
    } else {
      new_alive[p] = 0;
    }
  }
}
