## Installing Stack

If you don't have Stack installed in your system, follow the
[installation instructions](http://docs.haskellstack.org/en/stable/install_and_upgrade/)
for your operating system. After completing the install, test that you have
installed it correctly by running this command in a terminal:

```bash
stack --version
```

If it outputs Stack's version, then you have it installed correctly.

## Installing dependencies

You'll also need to install some additional packages in your *implicit
global project*:

```bash
stack setup
stack install attoparsec HUnit lens old-locale parallel QuickCheck random split stm text vector
```

## Installing HLint (optional)

HLint is a tool for suggesting possible improvements to Haskell code. These
suggestions include ideas such as using alternative functions, simplifying
code and spotting redundancies.

You can use Stack to install HLint:

```bash
stack install hlint
```

Check that it was installed correctly with:

```bash
hlint --version
```

If you get the version in your terminal, you're done, otherwise, you
probably need to add a missing directory to your path:

- `%APPDATA%\local\bin`, if you are running *Windows*
- `$HOME/.local/bin`, for other operating systems.
