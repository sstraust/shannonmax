# emacskeys

A library for analyzing keybindings in emacs.

WARNING: When using a keylogger, be careful not to log any sensitive information!


Known Limitations:
Certain packages mess with the keylogger's ability to correctly log commands. In particular, shannonmax uses real-last-command and this-command-keys-vector inside of post-command-hook in order to determine what keys/commands to log. Certain packages (like ido-mode interfere with our ability to correctly set these values). We use post(rather than pre)-command-hook so that logged keys are correctly set in god-mode.

If you have feedback on how to get around this, I'd love your thoughts and advice!


citing this work:

Straus, S. (2024). Better Keybindings with Information Theory [Computer software]. https://github.com/sstraust/shannonmax
