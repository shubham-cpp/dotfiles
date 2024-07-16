unsigned int
nexttag(void)
{
	unsigned int seltag = selmon->tagset[selmon->seltags];
	return seltag == (1 << (NUMTAGS - 1)) ? 1 : seltag << 1;
}

unsigned int
prevtag(void)
{
	unsigned int seltag = selmon->tagset[selmon->seltags];
	return seltag == 1 ? (1 << (NUMTAGS - 1)) : seltag >> 1;
}

void
tagtonext(const Arg *arg)
{
	unsigned int tmp;

	if (selmon->sel == NULL)
		return;

	tmp = nexttag();
	tag(&(const Arg){.ui = tmp });
	view(&(const Arg){.ui = tmp });
}

void
tagtoprev(const Arg *arg)
{
	unsigned int tmp;

	if (selmon->sel == NULL)
		return;

	tmp = prevtag();
	tag(&(const Arg){.ui = tmp });
	view(&(const Arg){.ui = tmp });
}

void
viewnext(const Arg *arg)
{
	view(&(const Arg){.ui = nexttag()});
}

void
viewprev(const Arg *arg)
{
	view(&(const Arg){.ui = prevtag()});
}
 
unsigned int
nexttag_skip(void)
{
	unsigned int seltag = selmon->tagset[selmon->seltags];
	unsigned int usedtags = 0;
	Client *c = selmon->cl->clients;

	if (!c)
		return seltag;

	/* skip vacant tags */
	do {
		usedtags |= c->tags;
		c = c->next;
	} while (c);

	do {
		seltag = seltag == (1 << (NUMTAGS - 1)) ? 1 : seltag << 1;
	} while (!(seltag & usedtags));

	return seltag;
}
unsigned int
prevtag_skip(void)
{
	unsigned int seltag = selmon->tagset[selmon->seltags];
	unsigned int usedtags = 0;
	Client *c = selmon->cl->clients;
	if (!c)
		return seltag;

	/* skip vacant tags */
	do {
		usedtags |= c->tags;
		c = c->next;
	} while (c);

	do {
		seltag = seltag == 1 ? (1 << (NUMTAGS - 1)) : seltag >> 1;
	} while (!(seltag & usedtags));

	return seltag;
}

void
tagtonext_skip(const Arg *arg)
{
	unsigned int tmp;

	if (selmon->sel == NULL)
		return;

	if ((tmp = nexttag_skip()) == selmon->tagset[selmon->seltags])
		return;

	tag(&(const Arg){.ui = tmp });
	view(&(const Arg){.ui = tmp });
}

void
tagtoprev_skip(const Arg *arg)
{
	unsigned int tmp;

	if (selmon->sel == NULL)
		return;

	if ((tmp = prevtag_skip()) == selmon->tagset[selmon->seltags])
		return;

	tag(&(const Arg){.ui = tmp });
	view(&(const Arg){.ui = tmp });
}

void
viewnext_skip(const Arg *arg)
{
	view(&(const Arg){.ui = nexttag_skip()});
}

void
viewprev_skip(const Arg *arg)
{
	view(&(const Arg){.ui = prevtag_skip()});
}
 
