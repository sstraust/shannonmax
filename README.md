# ShannonMax

ShannonMax uses information theory to **analyze your emacs usage** and **suggest better keybindings**.

In particular, it helps you find commands where changing the keybinding can <ins>significantly reduce the amount you type</ins>.

![Example Results](https://github.com/sstraust/shannonmax/blob/master/ExampleResults.png)


WARNING: When using a keylogger, be careful not to log any sensitive information!

Cite This:\
Straus, S. (2024). Better Keybindings with Information Theory [Computer software]. https://github.com/sstraust/shannonmax

Original Talk: https://www.youtube.com/watch?v=MytPttbIUOY

## How to Use
1. Download the files ``` shannon-max.el ```, and ``` target/emacskeys-0.1.0-SNAPSHOT-standalone.jar ```. Put them in the same directory, somewhere in your emacs path. So the structure should look like:
   ```
   ~/.emacs.d/custom/shannon-max.el
   ~/.emacs.d/custom/target/emacskeys-0.1.0-SNAPSHOT-standalone.jar
   ```
   
   And you should have something in your ```.emacs``` that looks like this:
	```
    (add-to-list 'load-path "~/.emacs.d/custom")
    (require 'shannon-max)
	(setq shannon-max-jar-file
      (expand-file-name "~/.emacs.d/custom/target/emacskeys-0.1.0-SNAPSHOT-standalone.jar"))

   ```
   You'll also need to have java/the jvm available on your machine and chmod +x the jar file to make it executable.

2. Start collecting data.
   
   Add 
        ``` (shannon-max-start-logger) ``` to your .emacs configuration file.
		
	After a few minutes, check the contents of ``` ~/emacs-logged-keys ``` to verify the logger is working correctly.

3. Once you have enough data, call ``` M-x shannon-max-analyze ``` to see the results!

## Viewing the Output

At any time, you can call ``` M-x shannon-max-analyze ``` to see the results.

In the results buffer:\
``` C-c C-n ``` to scroll down a page of results\
``` C-c C-p ``` to scroll up a page of results\
``` C-c C-e ``` to call keymap-global-set to globally bind a keysequence (emacs version 29 only)

#### Interpreting the Results

Shannonmax compares the _actual_ length of your keybinding (e.g. "x e" has length 2), to the _theoretical_ length of your keybinding (how long it _should_ be, given how much you actually use it). We use information theory on the logged data to determine a keybinding's theoretical length. 

Commands you use more often should have shorter keybindings, so in general, the strategy is to rebind "Keybindings that are too long" to something that's shorter and more convenient. If you run out of keys on your keyboard, you can free up space by unbinding infrequently used "Keybindings that are too short".

You can learn more about it by watching the video here: https://www.youtube.com/watch?v=MytPttbIUOY





## Customizing the Behavior

``` shannon-max-custom-keypress-cost ```

By default, shannon-max assumes every keypress costs ``` 1 ```. Sequences with control characters are given an additional cost of 1 per control character.

So for example:
* "a"     Has a cost of 1
* "a b"   Has a cost of 2
* "C-a"   Has a cost of 2
* "C-M-a" Has a cost of 3
* "C-x C-s" Has a cost of 4

If you have a different view of the world, you can write your own cost function.

If you create your own cost function, you'll also need to modify alphabet-size to the corresponding value.

```shannon-max-alphabet-size ```

This represents "How much can I type with a single keypress?". It's used to compute the theoretical length of a given key command.

For example, if you only have two keys on your keyboard, then in theory your keybindings must be very long, if you are to represent every possible command with just 2 keys. But if you have 100 keys on your keyboard, your keybindings can be very short.

By default, we assume you have 52 keys on your keyboard, and every keypress costs 1.


If you change your cost function, you need to calculate the correct alphabet size for your new cost-weighted "keyboard". The right way to do this is by solving the characteristic function as described here: https://people.math.harvard.edu/~ctm/home/text/others/shannon/entropy/entropy.pdf (page 3). I want to make this calculation more automatic, and it's planned for future work.

If you're too lazy to calculate it out you can fiddle with the value until it seems right, and you should still get somewhat useful/actionable results.


``` shannon-max-filtered-commands ```

A list of emacs commands to ignore from the output. By default we also filter all commands matching "lambda", "(", or "[". This is particularly useful for removing things like the self insert command.

``` shannon-max-filter-commands-fn ```

Basically does the same thing as shannon-max-filtered-commands, but as a function.

## Known Limitations

#### Keylogger Limitations

Certain packages mess with the keylogger's ability to correctly log commands. In particular, shannonmax uses real-last-command and this-command-keys-vector inside of post-command-hook in order to determine what keys/commands to log. Certain packages (like ido-mode) interfere with our ability to correctly set these values. We use post(rather than pre)-command-hook so that logged keys are correctly set in god-mode.

If you have feedback on how to get around this, I'd love your thoughts and advice!

#### Better Support For Major Modes
Certain keybindings are only active in major/minor modes. Ideally we'd compute separate keymaps for every major minor mode, but it's tricky/takes a lot of thought and care to handle this correctly in a way that's relevant and useful.

#### Support for Multi-Command Codings/Arithmetic Codings
To get really optimal output, we'd like to understand how _pairs_ or sequences of commonly used emacs commands can be folded into a single keybinding

#### More Things to Work On
- Easier Installation on MELPA
- Better Custom UI menus for rebinding keys (rather than keymap-global-set)
- Better support for capitalized letters


Any other thoughts? Feel free to open an issue on this Repo :D


