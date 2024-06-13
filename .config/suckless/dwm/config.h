/* See LICENSE file for copyright and license details. */

/* appearance */
static const int startwithgaps[]				 = { 1 };	/* 1 means gaps are used by default, this can be customized for each tag */
static const unsigned int gappx[]   		 = { 3 };   /* default gap between windows in pixels, this can be customized for each tag */
static const unsigned int borderpx       = 3;        /* border pixel of windows */
static const unsigned int snap           = 32;       /* snap pixel */
static const unsigned int systraypinning = 0;   /* 0: sloppy systray follows selected monitor, >0: pin systray to monitor X */
static const unsigned int systrayonleft  = 0;    /* 0: systray in the right corner, >0: systray on left of status text */
static const unsigned int systrayspacing = 2;   /* systray spacing */
static const int systraypinningfailfirst = 1;   /* 1: if pinning fails, display systray on the first monitor, False: display systray on the last monitor*/
static const int showsystray             = 1;        /* 0 means no systray */
static const int showbar                 = 1;        /* 0 means no bar */
static const int topbar                  = 1;        /* 0 means bottom bar */
static const Bool viewontag              = True;     /* Switch view on tag switch */
static const char *fonts[]               = { 
  "Fira Sans:size=12",
  "Symbols Nerd Font:size=13",
  "Noto Color Emoji:size=12"
};
static const char dmenufont[]            = "monospace:size=10";
// static const char col_gray1[]       = "#222222";
// static const char col_gray2[]       = "#444444";
static const char col_gray3[]       = "#767c85";
// static const char col_gray4[]       = "#eeeeee";
// static const char col_cyan[]        = "#005577";

static const char norm_fg[]     = "#f9f8f5";
static const char norm_bg[]     = "#2c323c";
static const char norm_border[] = "#75715e";

static const char sel_fg[]      = "#A9DC76";
static const char sel_bg[]      = "#444444";
static const char sel_border[]  = "#7aa2f7";

static const char urg_fg[]      = "#e5c06b";
static const char urg_bg[]      = "#FF6188";
static const char urg_border[]  = "#FF6188";

static const char *colors[][3]      = {
  /*               fg           bg         border                         */
  [SchemeNorm] = { norm_fg,     norm_bg,   norm_border }, // unfocused wins
  [SchemeSel]  = { sel_fg,      sel_bg,    sel_border },  // the focused win
  [SchemeHid]  = { urg_fg,      norm_bg,   norm_border },
  [SchemeHov]   = { sel_fg,    sel_bg,    sel_bg  },
  [SchemeStatus]= { sel_bg,    norm_bg,   NULL  },
};

/* status bar */
static const Block blocks[] = {
	/* fg     command				interval(secs)	signal(34+x) */
  {norm_fg, "(sleep 5 ; curl \"wttr.in/Pune?format=3\")", 1800, 10},
  {norm_fg,  "sb-bright", 0,  2},
  {norm_fg,  "sb-volume", 0,  3},
	{norm_fg,  "sb-mic",    0,  4},
	{norm_fg,  "sb-memory", 3,  5},
  {norm_fg,  "sb-load",   3,  6},
  {norm_fg,  "sb-battery",8, 7},
	{norm_fg,  "sb-date",   30, 1},
};

/* inverse the order of the blocks, comment to disable */
#define INVERSED	1
/* delimeter between blocks commands. NULL character ('\0') means no delimeter. */
static char delimiter[] = "    ";
/* max number of character that one block command can output */
#define CMDLENGTH	30

/* tagging */
static const char *tags[] = { "1", "2", "3", "4", "5", "6", "7", "8", "9" };

static const Rule rules[] = {
    /* xprop(1):
     *  WM_CLASS(STRING) = instance, class
     *  WM_NAME(STRING) = title
     */
    /* class              instance    title     tags mask   switchtotag    isfloating   monitor */
    { "floorp",            NULL,      NULL,       2,          1,            0,          -1 },
    { "Navigator",         NULL,      NULL,       2,          1,            0,          -1 },
    { "firefox",           NULL,      NULL,       2,          1,            0,          -1 },
    { "Firefox",           NULL,      NULL,       2,          1,            0,          -1 },
    { "LibreWolf",         NULL,      NULL,       2,          1,            0,          -1 },
    { "Brave-browser",     NULL,      NULL,       2,          1,            0,          -1 },
    { "Thorium-browser",   NULL,      NULL,       2,          1,            0,          -1 },
    { "thorium-browser",   NULL,      NULL,       2,          1,            0,          -1 },
    { "qutebrowser",       NULL,      NULL,       2,          1,            0,          -1 },
    { "Nyxt",              NULL,      NULL,       2,          1,            0,          -1 },
    { "Chromium",          NULL,      NULL,       2,          1,            0,          -1 },
    { "Chromium-browser",  NULL,      NULL,       2,          1,            0,          -1 },
    { "waterfox-current",  NULL,      NULL,       2,          1,            0,          -1 },

    { "thunderbird",       NULL,      NULL,       4,          1,            0,          -1 },
    { "slack",             NULL,      NULL,       4,          1,            0,          -1 },
    { "Steam",             NULL,      NULL,       4,          0,            0,          -1 },
    { "Lutris",            NULL,      NULL,       4,          1,            0,          -1 },
    { "Timeshift-gtk",     NULL,      NULL,       4,          1,            1,          -1 },

    { "Evolution",         NULL,      NULL,       4,          1,            0,          -1 },
    { "mpv",               NULL,      NULL,       4,          1,            0,          -1 },
    { "vlc",               NULL,      NULL,       4,          1,            0,          -1 },
    { "parole",            NULL,      NULL,       4,          1,            0,          -1 },
    { "smplayer",          NULL,      NULL,       4,          1,            0,          -1 },

    { "Virt-manager",      NULL,      NULL,       5,          1,            1,          -1 },
    { "VirtualBox Manager",NULL,      NULL,       5,          1,            1,          -1 },

    // Floating Rules
    {"Arandr",             NULL,      NULL,       0,          0,            1,          -1 },
    {"Arcologout.py",      NULL,      NULL,       0,          0,            1,          -1 },
    {"albert",             NULL,      NULL,       0,          0,            1,          -1 },
    {"feh",                NULL,      NULL,       0,          0,            1,          -1 },
    {"Galculator",         NULL,      NULL,       0,          0,            1,          -1 },
    {"Qalculate-gtk",      NULL,      NULL,       0,          0,            1,          -1 },
    {"Gnome-calculator",   NULL,      NULL,       0,          0,            1,          -1 },
    {"Nitrogen",           NULL,      NULL,       0,          0,            1,          -1 },
    {"Grub-customizer",    NULL,      NULL,       0,          0,            1,          -1 },
    {"Pavucontrol",        NULL,      NULL,       0,          0,            1,          -1 },
    {"pavucontrol",        NULL,      NULL,       0,          0,            1,          -1 },
    {"Minipad",            NULL,      NULL,       0,          0,            1,          -1 },
    {"Connman-gtk",        NULL,      NULL,       0,          0,            1,          -1 },

    /* xprop(1):
     *  WM_CLASS(STRING) = instance, class
     *  WM_NAME(STRING) = title
     */
    /* class              instance    title     tags mask   switchtotag    isfloating   monitor */
    {"Arcolinux-tweak-tool.py",   NULL, NULL,                                   0, 0, 1, -1 },
    {"Evolution-alarm-notify",    NULL, NULL,                                   0, 0, 1, -1 },
    {"tauri-sveltekit-notes-app", NULL, NULL,                                   0, 0, 1, -1 },
    {"Tauri-sveltekit-notes-app", NULL, NULL,                                   0, 0, 1, -1 },
    {"brave-browser",             NULL, "Sign in - Google Accounts - Brave",    0, 0, 1, -1 },
    {"Brave-browser",             NULL, "Sign in - Google Accounts - Brave",    0, 0, 1, -1 },
    {"Chromium-browser",          NULL, "Sign in - Google Accounts - Chromium", 0, 0, 1, -1 },
    {"thorium-browser",           NULL, "Sign in - Google Accounts - Thorium",  0, 0, 1, -1 },
    {"Chromium-browser",          NULL, "Sign in - Google Accounts - Chromium", 0, 0, 1, -1 },
    {"firefox",                   NULL, "Sign in - Google Accounts - Firefox",  0, 0, 1, -1 },
    {"floorp",                    NULL, "Sign in - Google Accounts - Floorp",   0, 0, 1, -1 },
    {"Thorium-browser",           NULL, "Bitwarden",                             0, 0, 1, -1 },
    {"thorium-browser",           NULL, "Bitwarden",                             0, 0, 1, -1 },
    {"Chromium-browser",          NULL, "Bitwarden",                             0, 0, 1, -1 },
    {"firefox",                   NULL, "Bitwarden",                             0, 0, 1, -1 },
    {"floorp",                    NULL, "Bitwarden",                             0, 0, 1, -1 },
    {"Firefox",                   NULL, "Bitwarden",                             0, 0, 1, -1 },
    {"Floorp",                    NULL, "Bitwarden",                             0, 0, 1, -1 },

};

/* layout(s) */
static const float mfact     = 0.51; /* factor of master area size [0.05..0.95] */
static const int nmaster     = 1;    /* number of clients in master area */
static const int resizehints = 1;    /* 1 means respect size hints in tiled resizals */
static const int lockfullscreen = 1; /* 1 will force focus on the fullscreen window */

static const Layout layouts[] = {
	/* symbol     arrange function */
	{ "[]=",      tile },    /* first entry is default */
	{ "><>",      NULL },    /* no layout function means floating behavior */
	{ "[M]",      monocle },
};

#include "movestack.c"

// Move the window to other monitor and switch the focus to it
void 
movemonfocus(const Arg *arg) {
	 tagmon(arg);
   focusmon(arg);
}
/* key definitions */
#define MODKEY Mod4Mask
#define TAGKEYS(KEY,TAG) \
	{ MODKEY,                       KEY,      view,           {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask,           KEY,      toggleview,     {.ui = 1 << TAG} }, \
	{ MODKEY|ShiftMask,             KEY,      tag,            {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask|ShiftMask, KEY,      toggletag,      {.ui = 1 << TAG} },

/* helper for spawning shell commands in the pre dwm-5.0 fashion */
#define SHCMD(cmd) { .v = (const char*[]){ "/bin/sh", "-c", cmd, NULL } }

/* commands */
static char dmenumon[2] = "0"; /* component of dmenucmd, manipulated in spawn() */
static const char *dmenucmd[] = { "dmenu_run_history", "-i", "-m", dmenumon , NULL };
static const char *termcmd[]  = { "st", NULL };

static const Key keys[] = {
	/* modifier                     key        function        argument */ /* Programs */
	{ MODKEY|ShiftMask,             XK_d,            spawn,          {.v = dmenucmd } },
	{ MODKEY,                       XK_Return,       spawn,          SHCMD("$TERMINAL || st") },
	{ MODKEY,                       XK_b,            togglebar,      {0} },
	{ Mod1Mask,                     XK_Tab,          focusstackvis,  {.i = +1 } },
	{ MODKEY,                       XK_j,            focusstackvis,  {.i = +1 } },
	{ MODKEY,                       XK_k,            focusstackvis,  {.i = -1 } },
	{ MODKEY|ShiftMask,             XK_j,            movestack,      {.i = +1 } },
	{ MODKEY|ShiftMask,             XK_k,            movestack,      {.i = -1 } },
	{ MODKEY,                       XK_i,            incnmaster,     {.i = +1 } },
	{ MODKEY|ShiftMask,             XK_i,            incnmaster,     {.i = -1 } },
	{ MODKEY,                       XK_h,            setmfact,       {.f = -0.05} },
	{ MODKEY,                       XK_l,            setmfact,       {.f = +0.05} },
	{ MODKEY,                       XK_bracketright, viewnext,       {0} },
	{ MODKEY,                       XK_bracketleft,  viewprev,       {0} },
	{ MODKEY|ShiftMask,             XK_bracketright, tagtonext,      {0} },
	{ MODKEY|ShiftMask,             XK_bracketleft,  tagtoprev,      {0} },
	{ MODKEY,                       XK_grave,        viewnext_skip,   {0} },
	{ MODKEY|ShiftMask,             XK_grave,        viewprev_skip,   {0} },
	{ MODKEY,                       XK_apostrophe,   viewnext_skip,   {0} },
	{ MODKEY,                       XK_semicolon,    viewprev_skip,   {0} },
	{ MODKEY,                       XK_Right,        viewnext_skip,   {0} },
	{ MODKEY,                       XK_Left,         viewprev_skip,   {0} },
	{ MODKEY|ShiftMask,             XK_Right,        tagtonext_skip,  {0} },
	{ MODKEY|ShiftMask,             XK_Left,         tagtoprev_skip,  {0} },
	{ MODKEY,                       XK_z,            zoom,           {0} },
	{ MODKEY,                       XK_Tab,          view,           {0} },
	{ MODKEY|ShiftMask,             XK_q,            killclient,     {0} },
	{ MODKEY,                       XK_t,            setlayout,      {.v = &layouts[0]} },
	{ MODKEY,                       XK_m,            setlayout,      {.v = &layouts[2]} },
	{ MODKEY,                       XK_space,        setlayout,      {0} },
	{ MODKEY,                       XK_s,            togglefloating, {0} },
	{ MODKEY|ShiftMask,             XK_s,            togglesticky,   {0} },
  { MODKEY,                       XK_f,            togglefullscr,  {0} },
	{ MODKEY,                       XK_0,            view,           {.ui = ~0 } },
	{ MODKEY|ShiftMask,             XK_0,            tag,            {.ui = ~0 } },
	{ MODKEY,                       XK_comma,        focusmon,       {.i = -1 } },
	{ MODKEY,                       XK_period,       focusmon,       {.i = +1 } },
	{ MODKEY|ShiftMask,             XK_comma,        movemonfocus,   {.i = -1 } },
	{ MODKEY|ShiftMask,             XK_period,       movemonfocus,   {.i = +1 } },
	{ MODKEY,                       XK_minus,        setgaps,        {.i = -5 } },
	{ MODKEY,                       XK_equal,        setgaps,        {.i = +5 } },
	{ MODKEY|ShiftMask,             XK_minus,        setgaps,        {.i = GAP_RESET } },
	{ MODKEY|ShiftMask,             XK_equal,        setgaps,        {.i = GAP_TOGGLE} },
	{ MODKEY|ControlMask,           XK_s,            show,           {0} },
	{ MODKEY|ControlMask|ShiftMask, XK_s,            showall,        {0} },
	{ MODKEY|ControlMask,           XK_h,            hide,           {0} },
	{ MODKEY|ControlMask,           XK_x,            quit,           {0} }, // quit dwm
  { MODKEY|ControlMask,           XK_r,            quit,           {1} }, // restart
	  TAGKEYS(XK_1, 0)
	  TAGKEYS(XK_2, 1)
	  TAGKEYS(XK_3, 2)
	  TAGKEYS(XK_4, 3)
	  TAGKEYS(XK_5, 4)
	  TAGKEYS(XK_6, 5)
	  TAGKEYS(XK_7, 6)
	  TAGKEYS(XK_8, 7)
	  TAGKEYS(XK_9, 8)
};

/* button definitions */
/* click can be ClkTagBar, ClkLtSymbol, ClkStatusText, ClkWinTitle, ClkClientWin, or ClkRootWin */
static const Button buttons[] = {
	/* click                event mask      button          function        argument */
	{ ClkTagBar,            MODKEY,         Button1,        tag,            {0} },
	{ ClkTagBar,            MODKEY,         Button3,        toggletag,      {0} },
	{ ClkWinTitle,          0,              Button1,        togglewin,      {0} },
	{ ClkWinTitle,          0,              Button2,        zoom,           {0} },

	{ ClkStatusText,        0,              Button1,        sendstatusbar,   {.i = 1 } },
	{ ClkStatusText,        0,              Button2,        sendstatusbar,   {.i = 2 } },
	{ ClkStatusText,        0,              Button3,        sendstatusbar,   {.i = 3 } },
	{ ClkStatusText,        0,              Button4,        sendstatusbar,   {.i = 4 } },
	{ ClkStatusText,        0,              Button5,        sendstatusbar,   {.i = 5 } },
	{ ClkStatusText,        ShiftMask,      Button1,        sendstatusbar,   {.i = 6 } },

	{ ClkClientWin,         MODKEY,         Button1,        movemouse,      {0} },
	{ ClkClientWin,         MODKEY,         Button2,        togglefloating, {0} },
	{ ClkClientWin,         MODKEY,         Button3,        resizemouse,    {0} },
	{ ClkTagBar,            0,              Button1,        view,           {0} },
	{ ClkTagBar,            0,              Button3,        toggleview,     {0} },
	{ ClkTagBar,            MODKEY,         Button1,        tag,            {0} },
	{ ClkTagBar,            MODKEY,         Button3,        toggletag,      {0} },
};

