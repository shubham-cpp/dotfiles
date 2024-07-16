void
movestack(const Arg *arg)
{
	Client *c = NULL, *p = NULL, *pc = NULL, *i;
	if (arg->i > 0) {
		if (!selmon->sel)
			return;
		/* find the client after selmon->sel */
		for (c = selmon->sel->next; c && (!ISVISIBLE(c, selmon) || c->isfloating); c = c->next);
		if (!c)
			for (c = selmon->cl->clients; c && (!ISVISIBLE(c, selmon) || c->isfloating); c = c->next);
	}
	else {
		/* find the client before selmon->sel */
		for (i = selmon->cl->clients; i != selmon->sel; i = i->next)
			if(ISVISIBLE(i, selmon) && !i->isfloating)
				c = i;
		if (!c)
			for (; i; i = i->next)
				if (ISVISIBLE(i, selmon) && !i->isfloating)
					c = i;
	}

	/* find the client before selmon->sel and c */
	for (i = selmon->cl->clients; i && (!p || !pc); i = i->next) {
		if (i->next == selmon->sel)
			p = i;
		if (i->next == c)
			pc = i;
	}

	/* swap c and selmon->sel selmon->clients in the selmon->clients list */
	if (c && c != selmon->sel) {
		Client *temp = selmon->sel->next==c?selmon->sel:selmon->sel->next;
		selmon->sel->next = c->next==selmon->sel?c:c->next;
		c->next = temp;

		if (p && p != c)
			p->next = c;
		if (pc && pc != selmon->sel)
			pc->next = selmon->sel;

		if (selmon->sel == selmon->cl->clients)
			selmon->cl->clients = c;
		else if (c == selmon->cl->clients)
			selmon->cl->clients = selmon->sel;

		arrange(selmon);
	}
}

