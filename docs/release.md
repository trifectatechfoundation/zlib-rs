# Release checklist

- [ ] Checkout the target commit for a release. On `main` (or other branches)
      make sure you pull the latest upstream commits.
- [ ] Set the new version as an environment variable: `export VERSION=[version]`
- [ ] In `Cargo.toml`, update the `workspace.package.version` key to the new
      version for all the crates (all crates have the same version).
- [ ] In `Cargo.toml` update the dependency version of `libz-rs-sys` and
      `zlib-rs` so that they have the same version (otherwise `libz-rs-sys` will
      attempt to compile with the older version).
- [ ] In `libz-rs-sys-cdylib/Cargo.toml` update the `package.version` key to the
      new version.
- [ ] In `libz-rs-sys-cdylib/Cargo.toml` update the dependency on `libz-rs-sys`
      to the new version.
- [ ] Run `cargo clean`
- [ ] Run `cargo build --release`
- [ ] Run `cargo test --release`
- [ ] `git commit -a -S -m "Release $VERSION"` (where `$VERSION` is the actual
      version to be released, making sure the commit is signed)
- [ ] Run `cargo publish --dry-run -p zlib-rs`
- [ ] Run `cargo publish -p zlib-rs`
- [ ] Run `cargo publish --dry-run -p libz-rs-sys`
- [ ] Run `cargo publish -p libz-rs-sys`
- [ ] Run `(cd libz-rs-sys-cdylib && cargo publish --dry-run)`
- [ ] Run `(cd libz-rs-sys-cdylib && cargo publish)`
- [ ] `git tag -a -s -m "Release $VERSION" "v$VERSION"` replacing both
      occurences of `$VERSION` with the version we are releasing
- [ ] Push the tag to the remote repository: `git push origin "v$VERSION"`
- [ ] If the commit was made on a branch, make sure to update the branch as
      well, i.e.: `git push origin main` when pushing the main branch
- [ ] Create a new release on the GitHub webinterface:
      https://github.com/trifectatechfoundation/zlib-rs/releases/new
      Choose the tag from the dropdown and click `Generate release notes`. If
      everything looks alright, publish the release (optionally setting
      pre-release and latest release if that is the case)
