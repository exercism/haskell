## Installing Stack

If you don't have a recent Stack version installed in your system, follow the
[installation instructions](http://docs.haskellstack.org/en/stable/install_and_upgrade/)
for your operating system. After completing the installation, test that you
have installed it correctly by running this command in a terminal:

```bash
stack --version
```

If it outputs a Stack version equal to or greater than 1.1.2, then you have it
installed correctly.

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
