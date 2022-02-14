#define CMDLENGTH 60
#define DELIMITER " | "
// #define CLICKABLE_BLOCKS
// Sending kill signal
// Just add 34 to your typical signal number
// kill -39 $(pidof dwmblocks)
const Block blocks[] = {
    /*      command                                  timeout signal*/
    BLOCK("curl -s v2.wttr.in | grep -e \"Weather\" | sed -n 2p | sed s/Weather://g | sed 's/,//g' | sed 's/+//g' | sed 's/°C.*/°C/' | sed 's/.*m//'", 60*60*3,    13),
    BLOCK("echo \"📅  $(date '+%a, %d %b - %I:%M')\"", 60, 12),
    BLOCK("dwm_net", 4, 11),
    BLOCK("echo \"  $(expr 100 - $(vmstat 1 2|tail -1|awk '{print $15}')) %\"", 3, 19),
    BLOCK("echo \"  $(free -mh --si | awk  {'print $3'} | head -n 2 | tail -1)\"", 3, 21),
    BLOCK("dwm_bat", 3, 14),
    BLOCK("echo \"  $(pamixer --get-volume-human  || echo )\"", 0, 10),
    BLOCK("echo \"  $(brightnessctl g)\"",0, 20),
};
