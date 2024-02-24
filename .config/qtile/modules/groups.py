from libqtile.config import EzKey as Key
from libqtile.config import Group
from libqtile.lazy import lazy

from .keys import keys

groups = [Group(i) for i in "123456789"]

for g in groups:
    keys.extend(
        [
            # mod1 + letter of group = switch to group
            Key(
                f"M-{g.name}",
                lazy.group[g.name].toscreen(),
                desc=f"Switch to group {g.name}",
            ),
            # mod1 + shift + letter of group = switch to & move focused window to group
            Key(
                f"M-S-{g.name}",
                lazy.window.togroup(g.name, switch_group=True),
                desc=f"Switch to & move focused window to group {g.name}",
            ),
            # Or, use below if you prefer not to switch to that group.
            # # mod1 + shift + letter of group = move focused window to group
            # Key([mod, "shift"], i.name, lazy.window.togroup(i.name),
            #     desc="move focused window to group {}".format(i.name)),
        ]
    )
