A command line too to get the list of loans on Saint-Paul bibliotheque.

In summary, this is a limited access to https://mediatheques-saintpaul.re, for
multiple account, allowing to list your books and how much time you can keep
them.

With a bit of setup, it should also work for any "Iguana" based library website, look for `iguana_root` in the code.

# Features:

- Automated login process for multiple accounts
- Recover the list of book and how much time you can keep them

# Usage:

Fill the `credentials.json` file as such:

```json
[
  {
    "credential": {"login": "X0000000001", "password": "01010101"},
    "displayName": "Laureline"
  },
  {
    "credential": {"login": "X0000000001", "password": "02020202"},
    "displayName": "Xena"
  }
]
```

Then run the executable (with `--refresh`). You can do that with nix:

```
# With nix, takes care of everything
nix run .# -- --refresh`

# with cabal
cabal run -- --refresh
```

The `--refresh` flag can be ignored on future run unless you want to refresh the listing.

![](assets/example.png)

# Android UI

- Create a `release-key.keystore` file:

```
nix shell nixpkgs#zulu.out
keytool -genkey -v -keystore release-key.keystore -alias alias -keyalg RSA -keysize 2048 -validity 10000
```

- Build the UI with: `nix build --impure path:.\#ui.android.st-paul-books`
- Deploy with `./result/bin/deploy` (if phone connect with `adb`) or just download the APK on the phone.


![](assets/android-ui.png)

You will need to enter the `credential.json` file content into the `Settings` panel.

# Future development

- Automatic time extension for book close to the delay
- Split auth and query, so we can reuse already authenticated users.
