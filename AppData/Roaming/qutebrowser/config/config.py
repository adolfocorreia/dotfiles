import os
config_home = os.getenv("XDG_CONFIG_HOME")
assert config_home is not None
with open(os.path.join(config_home, "qutebrowser/config.py")) as file:
    exec(file.read())
