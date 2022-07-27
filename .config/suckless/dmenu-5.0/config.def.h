/* See LICENSE file for copyright and license details. */
/* Default settings; can be overriden by command line. */

static int topbar = 1;                      /* -b  option; if 0, dmenu appears at bottom     */
static int fuzzy = 1;                      /* -F  option; if 0, dmenu doesn't use fuzzy matching     */
/* -fn option overrides fonts[0]; default X11 font or font set */
static const char *fonts[] = {
	"JetBrainsMono Nerd Font:size=14:antialias=true:autohint=true",
    "JoyPixels:size=14:antialias=true:autohint=true"
};
static const char *prompt      = NULL;      /* -p  option; prompt to the left of input field */
static const char *colors[SchemeLast][2] = {
	                      /*     fg          bg   */
	[SchemeNorm]          = { "#ebc6ca", "#09021b" },
	[SchemeSel]           = { "#ebc6ca", "#64318C" },
	[SchemeSelHighlight]  = { "#31b62f", "#005577" },
	[SchemeNormHighlight] = { "#31b62f", "#222222" },
	[SchemeOut]           = { "#ebc6ca", "#51BAED" },
};
/* -l and -g options; controls number of lines and columns in grid if > 0 */
static unsigned int lines      = 0;
static unsigned int columns    = 0;

/*
 * Characters not considered part of a word while deleting words
 * for example: " /?\"&[]"
 */
static const char worddelimiters[] = " ";
