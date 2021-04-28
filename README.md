# Screener

## Overview

This is a simple tool that takes screenshots of a bunch of URLs for different
devices with headless chromium.

## How to use

Download the repository, then install all dependencies with:
```
nix-shell
npm install
```

Then do:

```
spago run -a CONFIGFILE
```

There is an example config file under `screenshots/tweag.yaml`.
This will install any required libraries on first run.
