# Tips

## Save and load dock configuration

```sh
dconf dump /net/launchpad/plank/docks/ > docks.dconf

cat docks.dconf | dconf load /net/launchpad/plank/docks/
```

## Application menu

Plank loads the application menu from `~/.config/menus/applications.menu`.
Sample application menu files can be found at `/etc/xdg/menus`.
Edit the menu file with `alacarte`.
