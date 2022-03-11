#define CMDLENGTH 60
#define DELIMITER "   "
// Adds a leading delimiter to the statusbar, useful for powerline.
// #define LEADING_DELIMITER
#define CLICKABLE_BLOCKS

const Block blocks[] = {
    /*      Command      interval    signal*/
    BLOCK("echo \"  $(expr 100 - $(vmstat 1 2|tail -1|awk '{print $15}')) %\"", 3, 19),
    BLOCK("echo \"  $(free -mh --si | awk  {'print $3'} | head -n 2 | tail -1)\"", 3, 21),
	BLOCK("sb-volume",     0,         22),
    BLOCK("sb-bright",     0,         23),
	BLOCK("sb-battery",    5,         24),
    BLOCK("sb-network",    10, 13),
    BLOCK("dwm_net",    3, 14),
    BLOCK("echo \"  $(date '+%a, %d %b - %I:%M')\"", 60, 12),
};
