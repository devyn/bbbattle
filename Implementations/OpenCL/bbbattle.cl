#define topoint(x, y) ((y) * WIDTH + (x))

__kernel void step_bbbattle(__global const char *alive, __global const char *dying, __global char *new_alive) {
  const int x = get_global_id(0);
  const int y = get_global_id(1);

  const size_t p = topoint(x, y);

  const char neighbor0 = ((x != 0)         & (y != 0))          ? alive[topoint(x - 1, y - 1)] : 0;
  const char neighbor1 =                     (y != 0)           ? alive[topoint(x    , y - 1)] : 0;
  const char neighbor2 = ((x != WIDTH - 1) & (y != 0))          ? alive[topoint(x + 1, y - 1)] : 0;
  const char neighbor3 =  (x != 0)                              ? alive[topoint(x - 1, y    )] : 0;
  const char neighbor4 =  (x != WIDTH - 1)                      ? alive[topoint(x + 1, y    )] : 0;
  const char neighbor5 = ((x != 0)         & (y != HEIGHT - 1)) ? alive[topoint(x - 1, y + 1)] : 0;
  const char neighbor6 =                     (y != HEIGHT - 1)  ? alive[topoint(x    , y + 1)] : 0;
  const char neighbor7 = ((x != WIDTH - 1) & (y != HEIGHT - 1)) ? alive[topoint(x + 1, y + 1)] : 0;

  barrier(CLK_GLOBAL_MEM_FENCE); // CPUs like to have everything sync'd up before moving on

  if (alive[p] | dying[p]) {
    new_alive[p] = 0;
  } else {
    const int alive_neighbor_count = (neighbor0 != 0)
                                   + (neighbor1 != 0)
                                   + (neighbor2 != 0)
                                   + (neighbor3 != 0)
                                   + (neighbor4 != 0)
                                   + (neighbor5 != 0)
                                   + (neighbor6 != 0)
                                   + (neighbor7 != 0);

    if (alive_neighbor_count == 2) {
      const char different = neighbor0
                           ^ neighbor1
                           ^ neighbor2
                           ^ neighbor3
                           ^ neighbor4
                           ^ neighbor5
                           ^ neighbor6
                           ^ neighbor7;

      const char new_team  = neighbor0
                           | neighbor1
                           | neighbor2
                           | neighbor3
                           | neighbor4
                           | neighbor5
                           | neighbor6
                           | neighbor7;

      new_alive[p] = !different ? new_team : -1;
    } else {
      new_alive[p] = 0;
    }
  }
}
