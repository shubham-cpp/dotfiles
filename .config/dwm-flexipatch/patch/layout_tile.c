static void
tile(Monitor *m)
{
	unsigned int i, n;
	int mx = 0, my = 0, mh = 0, mw = 0;
	int sx = 0, sy = 0, sh = 0, sw = 0;
	float mfacts, sfacts;
	int mrest, srest;
	Client *c;


	#if VANITYGAPS_PATCH
	int oh, ov, ih, iv;
	getgaps(m, &oh, &ov, &ih, &iv, &n);
	#else
	for (n = 0, c = nexttiled(m->clients); c; c = nexttiled(c->next), n++);
	#endif // VANITYGAPS_PATCH

	if (n == 0)
		return;

	#if VANITYGAPS_PATCH
	sx = mx = m->wx + ov;
	sy = my = m->wy + oh;
	mh = m->wh - 2*oh - ih * (MIN(n, m->nmaster) - 1);
	sh = m->wh - 2*oh - ih * (n - m->nmaster - 1);
	sw = mw = m->ww - 2*ov;

	if (m->nmaster && n > m->nmaster) {
		sw = (mw - iv) * (1 - m->mfact);
		mw = (mw - iv) * m->mfact;
		sx = mx + mw + iv;
	}
	#else
	sx = mx = m->wx;
	sy = my = m->wy;
	sh = mh = m->wh;
	sw = mw = m->ww;

	if (m->nmaster && n > m->nmaster) {
		sw = mw * (1 - m->mfact);
		mw = mw * m->mfact;
		sx = mx + mw;
	}
	#endif // VANITYGAPS_PATCH

	getfacts(m, mh, sh, &mfacts, &sfacts, &mrest, &srest);

	for (i = 0, c = nexttiled(m->cl->clients, m); c; c = nexttiled(c->next, m), i++)
		if (i < m->nmaster) {
			#if CFACTS_PATCH
			resize(c, mx, my, mw - (2*c->bw), (mh / mfacts) * c->cfact + (i < mrest ? 1 : 0) - (2*c->bw), 0);
			#else
			resize(c, mx, my, mw - (2*c->bw), (mh / mfacts) + (i < mrest ? 1 : 0) - (2*c->bw), 0);
			#endif // CFACTS_PATCH
			#if VANITYGAPS_PATCH
			my += HEIGHT(c) + ih;
			#else
			my += HEIGHT(c);
			#endif
		} else {
			#if CFACTS_PATCH
			resize(c, sx, sy, sw - (2*c->bw), (sh / sfacts) * c->cfact + ((i - m->nmaster) < srest ? 1 : 0) - (2*c->bw), 0);
			#else
			resize(c, sx, sy, sw - (2*c->bw), (sh / sfacts) + ((i - m->nmaster) < srest ? 1 : 0) - (2*c->bw), 0);
			#endif // CFACTS_PATCH
			#if VANITYGAPS_PATCH
			sy += HEIGHT(c) + ih;
			#else
			sy += HEIGHT(c);
			#endif
		}
}

