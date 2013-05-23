__kernel void step_bbbattle(__global const char *alive, __global const char *dying, __global char *new_alive, const uint width, const uint height) {
  const int x = get_global_id(0);
  const int y = get_global_id(1);

  const size_t p = y*width + x;

  if (alive[p] != 0 || dying[p] != 0) {
    new_alive[p] = 0;
  } else {
    int  nx, ny;
    int  alive_neighbor_count = 0;
    char new_team = 0;

    for (ny = -1; ny <= 1; ny++) {
      for (nx = -1; nx <= 1; nx++) {
        if (!(nx == 0 && ny == 0) && y + ny >= 0 && y + ny < height && x + nx >= 0 && x + nx < width) {
          if (alive[(y + ny)*width + (x + nx)] != 0) {
            alive_neighbor_count++;

            if (new_team != -1) {
              if (new_team != 0) {
                if (new_team != alive[(y + ny)*width + (x + nx)]) {
                  new_team = -1;
                }
              } else {
                new_team = alive[(y + ny)*width + (x + nx)];
              }
            }
          }
        }
      }
    }

    if (alive_neighbor_count == 2) {
      new_alive[p] = new_team;
    } else {
      new_alive[p] = 0;
    }
  }
}
