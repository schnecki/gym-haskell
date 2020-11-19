# Gym Haskell

Minimal gym bindings for Haskell, see https://gym.openai.com

## REQUIREMENTS

 - python 3.8 and
 - gym (https://gym.openai.com/docs/#installation)

### ArchLinux Commands:

    $ yay -S python38                # for yay see https://wiki.archlinux.org/index.php/AUR_helpers
    $ curl https://bootstrap.pypa.io/get-pip.py -o get-pip.py
    $ python3.8 get-pip.py --user
    $ pip3.8 install gym --user
    $ stack build
