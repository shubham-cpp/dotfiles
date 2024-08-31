/* static void cyclefocussame(const Arg *arg); */

int
is_match(Client *c, const char *class_name)
{
    XClassHint ch = { NULL, NULL };
    int match = 0;

    if (ISVISIBLE(c, selmon) && !HIDDEN(c) && XGetClassHint(dpy, c->win, &ch)) {
        match = (strcmp(class_name, ch.res_class) == 0);
        XFree(ch.res_class);
        XFree(ch.res_name);
    }
    return match;
}

void
cyclefocussame(const Arg *arg)
{
  Client *c = NULL, *i;
  XClassHint ch = { NULL, NULL };
  char *class_name = NULL;
  int inc = arg->i;

  if (!selmon->sel || !XGetClassHint(dpy, selmon->sel->win, &ch))
    return;
  class_name = ch.res_class;

  if (inc > 0) {
    for (c = selmon->sel->next; c && !is_match(c, class_name); c = c->next);
    if (!c)
      for (c = selmon->cl->clients; c && c != selmon->sel && !is_match(c, class_name); c = c->next);
  } else {
    for (i = selmon->cl->clients; i && i != selmon->sel; i = i->next)
      if (is_match(i, class_name))
        c = i;
    if (!c)
      for (; i; i = i->next)
        if (is_match(i, class_name))
          c = i;
  }

  if (c) {
    focus(c);
    restack(selmon);
  }

  XFree(ch.res_class);
  XFree(ch.res_name);
}

