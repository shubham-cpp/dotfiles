## Role

Senior Wayland/Quickshell engineer. Minimal diffs. Match existing QML style.

## Non-negotiables

1. One Quickshell process. Overlays are Loader { active: false } until opened.
2. Do not run a second notification daemon or a second idle daemon.
3. Phase A: stasis decides WHEN to lock; WlSessionLock is the locker. SetLockedHint only after secure.
4. Caffeine = IdleInhibitor on the bar + logind Inhibit("idle","block") fd.
5. Clipboard persistence is cliphist. Skip x-kde-passwordManagerHint / CLIPBOARD_STATE=sensitive / ignored window classes. Evict last item when clipboard is cleared.
6. OSD is not a notification.
7. Reminders: one-shot timer to next deadline + systemd-run --user Persistent=true. No interval poll.
8. Launch qs with jemalloc decay MALLOC_CONF.
9. If unsure of a Quickshell API, read docs or source rather than inventing properties.

## Layout

- Imports: `qs.Common`, `qs.Services`, `qs.Modules.<name>`. No `root:/`, no committed `qmldir`.
- `IpcHandler` lives on Services (or `shell.qml` for `shell.ping`), never inside a Loader.
- `WlSessionLock` stays in the tree (`locked: false`). QS creates surfaces when locking.
- Once those slices land: NotificationServer, toast window, and OSD window stay always-on. Center / launcher / clipboard / calendar are Loaders.
- Singletons: `pragma Singleton` + root `Singleton { }`. `shell.qml` must reference always-on services or they never construct.
- One `Tokens.qml`. Clock is `SystemClock`. No `/proc` Process polls.

## Compositor

Mangowc first. Workspaces: `Quickshell.WindowManager` (`ext-workspace-v1`); occupancy via `mmsg watch` if needed. Task list: `ToplevelManager`. No `Quickshell.Hyprland`, no `hyprctl`.

## Research

Cited map: `research/00-INDEX.md`. Stasis lock contract: `research/10-stasis-quickshell-lock.md`.

## Output

Short plan, then patches. After a slice: how to run it and what was not done.
