#define CMDLENGTH 45
#define DELIMITER " | "
// #define CLICKABLE_BLOCKS

const Block blocks[] = {
    //       cmd      time signal
    BLOCK("sb-date",   60, 23),
    BLOCK("sb-memory", 5, 20),
    BLOCK("sb-load",   5, 21),
    BLOCK("sb-volume", 0, 18),
    BLOCK("sb-bright", 0, 19),
    BLOCK("sb-battery",5, 24),
};
