/* See LICENSE file for copyright and license details. */

/* appearance */
#define ICONSIZE 16   /* icon size */
#define ICONSPACING 5 /* space between icon and title */
static const unsigned int borderpx  = 3;        /* border pixel of windows */
static const int startwithgaps	     = 1;	 /* 1 means gaps are used by default */
static const unsigned int gappx     = 4;       /* default gap between windows in pixels */
static const unsigned int snap      = 32;       /* snap pixel */
static const unsigned int systraypinning = 0;   /* 0: sloppy systray follows selected monitor, >0: pin systray to monitor X */
static const unsigned int systrayonleft = 0;   	/* 0: systray in the right corner, >0: systray on left of status text */
static const unsigned int systrayspacing = 2;   /* systray spacing */
static const int systraypinningfailfirst = 1;   /* 1: if pinning fails, display systray on the first monitor, False: display systray on the last monitor*/
static const int showsystray        = 1;     /* 0 means no systray */
static const int showbar            = 1;     /* 0 means no bar */
static const int topbar             = 1;     /* 0 means bottom bar */
static const char *fonts[]          = {
    "Ubuntu:size=10",
    "Hack Nerd Font:size=12",
    "JoyPixels:size=12"
};
static const char dmenufont[]       = "monospace:size=10";
static const char col_gray1[]		= "#222222";
static const char col_gray2[]		= "#444444";
static const char col_gray3[]		= "#bbbbbb";
static const char col_gray4[]		= "#eeeeee";
static const char col_cyan[]		= "#4c515c";
static const char *colors[][3]      = {
	/*               fg         bg         border   */
	[SchemeNorm] = { col_gray3, col_gray1, col_gray2 },
	[SchemeSel]  = { col_gray4, col_cyan ,  "#e95678"},
};

/* tagging */
static const char *tags[] = { "1", "2", "3", "4", "5", "6", "7", "8", "9" };

static const Rule rules[] = {
	/* xprop(1):
	 *	WM_CLASS(STRING) = instance, class
	 *	WM_NAME(STRING) = title
	 */
	/* class			  instance	  title		tags mask	switchtotag    isfloating	monitor */
	{ "firefox",		   NULL,	  NULL,		  2,		  1,			0,			-1 },
	{ "Firefox",		   NULL,	  NULL,		  2,		  1,			0,			-1 },
	{ "LibreWolf",		   NULL,	  NULL,		  2,		  1,			0,			-1 },
	{ "Brave-browser",	   NULL,	  NULL,		  2,		  1,			0,			-1 },
	{ "qutebrowser",	   NULL,	  NULL,		  2,		  1,			0,			-1 },
	{ "Nyxt",			   NULL,	  NULL,		  2,		  1,			0,			-1 },
	{ "Chromium",		   NULL,	  NULL,		  2,		  1,			0,			-1 },
	{ "Chromium-browser",  NULL,	  NULL,		  2,		  1,			0,			-1 },
	{ "waterfox-current",  NULL,	  NULL,		  2,		  1,			0,			-1 },

	{ "Steam",			   NULL,	  NULL,		  4,		  0,			1,			-1 },
	{ "Lutris",			   NULL,	  NULL,		  4,		  0,			1,			-1 },
	{ "Timeshift-gtk",	   NULL,	  NULL,		  4,		  1,			1,			-1 },

	{ "Evolution",		   NULL,	  NULL,		  5,		  1,			0,			-1 },
	{ "mpv",			   NULL,	  NULL,		  5,		  1,			0,			-1 },
	{ "vlc",			   NULL,	  NULL,		  5,		  1,			0,			-1 },
	{ "parole",			   NULL,	  NULL,		  5,		  1,			0,			-1 },
	{ "smplayer",		   NULL,	  NULL,		  5,		  1,			0,			-1 },

	{ "Virt-manager",	   NULL,	  NULL,		  7,		  1,			1,			-1 },
	{ "VirtualBox Manager",NULL,	  NULL,		  7,		  1,			1,			-1 },

	// Floating Rules
	{"Arandr",			   NULL,	  NULL,		  0,		  0,			1,			-1 },
	{"Arcologout.py",	   NULL,	  NULL,		  0,		  0,			1,			-1 },
	{"albert",			   NULL,	  NULL,		  0,		  0,			1,			-1 },
	{"feh",				   NULL,	  NULL,		  0,		  0,			1,			-1 },
	{"Galculator",		   NULL,	  NULL,		  0,		  0,			1,			-1 },
	{"Nitrogen",		   NULL,	  NULL,		  0,		  0,			1,			-1 },
	{"Grub-customizer",    NULL,	  NULL,		  0,		  0,			1,			-1 },
	{"Pavucontrol",		   NULL,	  NULL,		  0,		  0,			1,			-1 },
	{"Minipad",			   NULL,	  NULL,		  0,		  0,			1,			-1 },
	{"Connman-gtk",		   NULL,	  NULL,		  0,		  0,			1,			-1 },
	{"Gnome-calculator",   NULL,	  NULL,		  0,		  0,			1,			-1 },
	{"Arcolinux-tweak-tool.py",NULL,NULL,		  0,		  0,			1,			-1 },
	{"Evolution-alarm-notify",NULL,NULL,		  0,		  0,			1,			-1 },
};

/* layout(s) */
static const float mfact     = 0.52; /* factor of master area size [0.05..0.95] */
static const int nmaster     = 1;    /* number of clients in master area */
static const int resizehints = 0;    /* 1 means respect size hints in tiled resizals */
static const int lockfullscreen = 1; /* 1 will force focus on the fullscreen window */

static const Layout layouts[] = {
	/* symbol     arrange function */
	{ "[[ T ]]",      tile },    /* first entry is default */
	{ "[[ F ]]",      NULL },    /* no layout function means floating behavior */
	{ "[[ M ]]",      monocle },
};

/* key definitions */
#define MODKEY Mod4Mask
#define TAGKEYS(KEY,TAG) \
	{ MODKEY,                       KEY,      view,           {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask,           KEY,      toggleview,     {.ui = 1 << TAG} }, \
	{ MODKEY|ShiftMask,             KEY,      tag,            {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask|ShiftMask, KEY,      toggletag,      {.ui = 1 << TAG} },

/* helper for spawning shell commands in the pre dwm-5.0 fashion */
#define SHCMD(cmd) { .v = (const char*[]){ "/bin/sh", "-c", cmd, NULL } }

#define STATUSBAR "dwmblocks"

/* commands */
static char dmenumon[2] = "0"; /* component of dmenucmd, manipulated in spawn() */
static const char *dmenucmd[] = { "dmenu_run_history", "-i", NULL };
static const char *termcmd[]  = { "st", NULL };

#include "movestack.c"
static Key keys[] = {
	/* modifier                 key                 function        argument */
	{ MODKEY,                   XK_d,               spawn,          {.v = dmenucmd } },
	{ MODKEY,                   XK_Return,          spawn,          {.v = termcmd } },
	{ MODKEY,                   XK_b,               togglebar,      {0} },

	{ MODKEY,                   XK_j,               focusstack,     {.i = +1 } },
	{ MODKEY,                   XK_k,               focusstack,     {.i = -1 } },
	{ MODKEY|ShiftMask,         XK_j,               movestack,      {.i = +1 } },
	{ MODKEY|ShiftMask,         XK_k,               movestack,      {.i = -1 } },
	{ MODKEY,                   XK_Up,              focusstack,     {.i = +1 } },
	{ MODKEY,                   XK_Down,            focusstack,     {.i = -1 } },
	{ MODKEY|ShiftMask,         XK_Up,              movestack,      {.i = +1 } },
	{ MODKEY|ShiftMask,         XK_Down,            movestack,      {.i = -1 } },

	{ MODKEY,                   XK_h,               setmfact,       {.f = -0.05} },
	{ MODKEY,                   XK_l,               setmfact,       {.f = +0.05} },
	{ MODKEY,                   XK_Left,            setmfact,       {.f = -0.05} },
	{ MODKEY,                   XK_Right,           setmfact,       {.f = +0.05} },

	{ MODKEY,				    XK_bracketright,    viewnext,			  {0} },
	{ MODKEY,				    XK_bracketleft,	    viewprev,			  {0} },
	{ MODKEY|ShiftMask,		    XK_bracketright,    tagtonext,			  {0} },
	{ MODKEY|ShiftMask,		    XK_bracketleft,	    tagtoprev,			  {0} },

	{ MODKEY,				   XK_grave,		 viewnext_skip,		  {0} },
	{ MODKEY|ShiftMask,		   XK_grave,		 viewprev_skip,		  {0} },
	{ MODKEY,				   XK_period,		 viewprev_skip,		  {0} },
	{ MODKEY,				   XK_comma,		 viewprev_skip,		  {0} },
	{ MODKEY|ShiftMask,		   XK_period,		 tagtonext_skip,	  {0} },
	{ MODKEY|ShiftMask,		   XK_comma,		 tagtoprev_skip,	  {0} },

	{ MODKEY,                   XK_i,               incnmaster,     {.i = +1 } },
	{ MODKEY|ShiftMask,         XK_i,               incnmaster,     {.i = -1 } },
	// { MODKEY,                   XK_Return, zoom,           {0} },
	{ MODKEY,                   XK_Tab,             view,           {0} },

	{ MODKEY|ShiftMask,         XK_q,               killclient,     {0} },

	{ MODKEY,                   XK_t,               setlayout,      {.v = &layouts[0]} },
	{ MODKEY,                   XK_s,               setlayout,      {.v = &layouts[1]} },
	{ MODKEY,                   XK_m,               setlayout,      {.v = &layouts[2]} },
	// { MODKEY,                   XK_space,  setlayout,      {0} },
	{ MODKEY,                   XK_space,           togglefloating, {0} },
	{ MODKEY,                   XK_f,               togglefullscr,  {0} },
	{ MODKEY,                   XK_0,               view,           {.ui = ~0 } },
	{ MODKEY|ShiftMask,         XK_0,               tag,            {.ui = ~0 } },
	// { MODKEY,                   XK_comma,           focusmon,       {.i = -1 } },
	// { MODKEY,                   XK_period,          focusmon,       {.i = +1 } },
	// { MODKEY|ShiftMask,         XK_comma,           tagmon,         {.i = -1 } },
	// { MODKEY|ShiftMask,         XK_period,          tagmon,         {.i = +1 } },

	{ MODKEY,                   XK_minus,           setgaps,        {.i = -5 } },
	{ MODKEY,                   XK_equal,           setgaps,        {.i = +5 } },
	{ MODKEY|ShiftMask,         XK_minus,           setgaps,        {.i = GAP_RESET } },
	{ MODKEY|ShiftMask,         XK_equal,           setgaps,        {.i = GAP_TOGGLE} },

	TAGKEYS(                    XK_1,                      0)
	TAGKEYS(                    XK_2,                      1)
	TAGKEYS(                    XK_3,                      2)
	TAGKEYS(                    XK_4,                      3)
	TAGKEYS(                    XK_5,                      4)
	TAGKEYS(                    XK_6,                      5)
	TAGKEYS(                    XK_7,                      6)
	TAGKEYS(                    XK_8,                      7)
	TAGKEYS(                    XK_9,                      8)

	{ MODKEY|ControlMask,       XK_x,               quit,           {0} },
    { MODKEY|ControlMask,       XK_r,               quit,           {1} },
};

/* button definitions */
/* click can be ClkTagBar, ClkLtSymbol, ClkStatusText, ClkWinTitle, ClkClientWin, or ClkRootWin */
static Button buttons[] = {
	/* click                event mask      button          function        argument */
	{ ClkTagBar,            MODKEY,         Button1,        tag,            {0} },
	{ ClkTagBar,            MODKEY,         Button3,        toggletag,      {0} },
	{ ClkWinTitle,          0,              Button2,        zoom,           {0} },
	{ ClkStatusText,        0,              Button1,        sigstatusbar,   {.i = 1} },
	{ ClkStatusText,        0,              Button2,        sigstatusbar,   {.i = 2} },
	{ ClkStatusText,        0,              Button3,        sigstatusbar,   {.i = 3} },
	{ ClkClientWin,         MODKEY,         Button1,        movemouse,      {0} },
	{ ClkClientWin,         MODKEY,         Button2,        togglefloating, {0} },
	{ ClkClientWin,         MODKEY,         Button3,        resizemouse,    {0} },
	{ ClkTagBar,            0,              Button1,        view,           {0} },
	{ ClkTagBar,            0,              Button3,        toggleview,     {0} },
	{ ClkTagBar,            MODKEY,         Button1,        tag,            {0} },
	{ ClkTagBar,            MODKEY,         Button3,        toggletag,      {0} },
};
