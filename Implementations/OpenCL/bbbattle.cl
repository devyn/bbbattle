#define topoint(x, y) ((y) * WIDTH + (x))

__kernel void step_bbbattle(__global const char *alive, __global const char *dying, __global char *new_alive) {
  const int x = get_global_id(0);
  const int y = get_global_id(1);

  const size_t p = y*WIDTH + x;

  char neighbors[8];

  neighbors[0] = ((x != 0)     & (y != 0))      ? alive[topoint(x - 1, y - 1)] : 0;
  neighbors[1] =                 (y != 0)       ? alive[topoint(x    , y - 1)] : 0;
  neighbors[2] = ((x != WIDTH) & (y != 0))      ? alive[topoint(x + 1, y - 1)] : 0;
  neighbors[3] =  (x != 0)                      ? alive[topoint(x - 1, y    )] : 0;
  neighbors[4] =  (x != WIDTH)                  ? alive[topoint(x + 1, y    )] : 0;
  neighbors[5] = ((x != 0)     & (y != HEIGHT)) ? alive[topoint(x - 1, y + 1)] : 0;
  neighbors[6] =                 (y != HEIGHT)  ? alive[topoint(x    , y + 1)] : 0;
  neighbors[7] = ((x != WIDTH) & (y != HEIGHT)) ? alive[topoint(x + 1, y + 1)] : 0;

  barrier(CLK_GLOBAL_MEM_FENCE); // CPUs like to have everything sync'd up before moving on

  if (alive[p] | dying[p]) {
    new_alive[p] = 0;
  } else {
    int  alive_neighbor_count = (neighbors[0] != 0)
                              + (neighbors[1] != 0)
                              + (neighbors[2] != 0)
                              + (neighbors[3] != 0)
                              + (neighbors[4] != 0)
                              + (neighbors[5] != 0)
                              + (neighbors[6] != 0)
                              + (neighbors[7] != 0);

    if (alive_neighbor_count == 2) {
      char different = neighbors[0]
                     ^ neighbors[1]
                     ^ neighbors[2]
                     ^ neighbors[3]
                     ^ neighbors[4]
                     ^ neighbors[5]
                     ^ neighbors[6]
                     ^ neighbors[7];

      char new_team  = neighbors[0]
                     | neighbors[1]
                     | neighbors[2]
                     | neighbors[3]
                     | neighbors[4]
                     | neighbors[5]
                     | neighbors[6]
                     | neighbors[7];

      new_alive[p] = !different ? new_team : -1;
    } else {
      new_alive[p] = 0;
    }
  }
}
