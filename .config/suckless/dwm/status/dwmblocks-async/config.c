#include "config.h"

#include "block.h"
#include "util.h"

Block blocks[] = {
  // To refresh all the blocks, run kill -10 $(pidof dwmblocks) or pkill
  // -SIGUSR1 dwmblocks.
  // If signal is 5, then kill -39 $(pidof dwmblocks) - 34 + 5(signal) = 39
  // command interval signal
  // {"sb-mail",    600,  1 },
  // {"sb-music",   0,    2 },
  // {"sb-disk",    1800, 3 },
    {"sb-memory", 10, 1 },
    {"sb-load",   5,  2 },
 // {"sb-mic",      0,    6 },
  // {"sb-record",   0,    7 },
    {"sb-volume", 0,  3 },
 // {"sb-battery",  5,    9 },
    {"sb-date",   60, 10},
};

const unsigned short blockCount = LEN(blocks);
