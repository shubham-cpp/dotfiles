void
hide(Client *c) {

	Client *n;
	if (!c || HIDDEN(c))
		return;

	Window w = c->win;
	static XWindowAttributes ra, ca;

	// more or less taken directly from blackbox's hide() function
	XGrabServer(dpy);
	XGetWindowAttributes(dpy, root, &ra);
	XGetWindowAttributes(dpy, w, &ca);
	// prevent UnmapNotify events
	XSelectInput(dpy, root, ra.your_event_mask & ~SubstructureNotifyMask);
	XSelectInput(dpy, w, ca.your_event_mask & ~StructureNotifyMask);
	XUnmapWindow(dpy, w);
	setclientstate(c, IconicState);
	XSelectInput(dpy, root, ra.your_event_mask);
	XSelectInput(dpy, w, ca.your_event_mask);
	XUngrabServer(dpy);

	if (c->isfloating || !c->mon->lt[c->mon->sellt]->arrange) {
		for (n = c->snext; n && (!ISVISIBLE(n, selmon) || HIDDEN(n)); n = n->snext);
		if (!n)
			for (n = c->mon->cl->stack; n && (!ISVISIBLE(n, selmon) || HIDDEN(n)); n = n->snext);
	} else {
		n = nexttiled(c, selmon);
		if (!n)
			n = prevtiled(c, selmon);
	}
	focus(n);
	arrange(c->mon);
}

void
show(Client *c)
{
	if (!c || !HIDDEN(c))
		return;

	XMapWindow(dpy, c->win);
	setclientstate(c, NormalState);
	arrange(c->mon);
}

void
togglewin(const Arg *arg)
{
	Client *c = (Client*)arg->v;
	if (!c)
		return;
	if (!HIDDEN(c) && c == selmon->sel)
		hide(c);
	else {
		if (HIDDEN(c))
			show(c);
		focus(c);
		restack(c->mon);
	}
}

Client *
prevtiled(Client *c, Monitor *m)
{
	Client *p, *i;
	for (p = NULL, i = c->mon->cl->clients; c && i != c; i = i->next)
		if (ISVISIBLE(i, m) && !HIDDEN(i))
			p = i;
	return p;
}

void
showhideclient(const Arg *arg)
{
	Client *c = (Client*)arg->v;
	if (!c)
		c = selmon->sel;
	if (!c)
		return;

	#if WARP_PATCH
	force_warp = 1;
	#endif // WARP_PATCH
	if (HIDDEN(c)) {
		show(c);
		focus(c);
		restack(c->mon);
	} else {
		hide(c);
	}
}

