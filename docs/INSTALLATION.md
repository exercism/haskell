# Installation


## Get GHCup

The recommended way of installing Haskell tools is through [GHCup][ghcup].

With GHCup you can manage your installed compilers, project/package managers, and language server.


## Install Stack

For doing Exercism exercises you only need Stack.

To install Stack, run the GHCup installation command or script.
Follow the defaults but specify that you would like to have Stack installed as well.

Did you by accident skip installation of Stack?
No worries: just run `ghcup install stack` afterwards.

GHCup will install

- **A compiler** (GHC: the Glasgow Haskell Compiler)

  Strictly speaking you do not need this for Exercism, but it comes in handy when you want to play with [GHCi][ghc-guide-ghci] outside of exercise directories.

- **Cabal**

  A project/package manager.
  You do not need this for Exercism.

- **Stack**

  The project/package manager that you will use for Exercism.
  It will make sure you have the right compiler and libraries for each individual project (exercise).

To check that installation was successful, run either of the commands

- `ghcup list`: success looks like a long list of tool versions, with Stack marked as installed
- `stack --version`: success looks like `Version 2.7.5, Git revision ...`


## Get the right GHC version

Stack will automatically download and install GHC when it is required.
You can start the installation and verify by running the following commands in a terminal:

```bash
# move into exercise directory
cd /exercism/haskell/hello-world
stack ghc -- --version
```

If this outputs something like

```
The Glorious Glasgow Haskell Compilation System, version 8.8.4
```

then GHC is installed correctly.


## Set up your editor to work with Haskell

Haskell support is provided by the Haskell Language Server (HLS).
Language support is not required, but strongly recommended.


### Visual Studio Code (recommended)

Just get the [Haskell extension for Visual Studio Code][vscode-haskell].
It will use GHCup to install HLS automatically, after which you'll be all set.

**Note:** with freshly downloaded exercises, HLS may have trouble starting up.
If this happens, just run `stack test` and restart the language server.


### Other editors

See the [HLS documentation on editor configuration][hls-editor-config].
You might need to install HLS manually.
To do so, simply run `ghcup install hls`.


[ghc-guide-ghci]:
    https://downloads.haskell.org/ghc/latest/docs/users_guide/ghci.html
    "GHC User’s Guide: Using GHCi"
[ghcup]:
    https://www.haskell.org/ghcup/
    "Get GHCup"
[hls-editor-config]:
    https://haskell-language-server.readthedocs.io/en/latest/configuration.html#configuring-your-editor
    "Editor configuration for Haskell Language Server"
[vscode-haskell]:
    https://marketplace.visualstudio.com/items?itemName=haskell.haskell
    "Haskell extension for VS Code"
