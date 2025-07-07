#!/bin/bash

# We use nightly across all benchmarks for consistency.

cargo +nightly build --examples --release
cp "target/release/examples/blogpost-uncompress" "target/release/nightly"

RUSTFLAGS="-Cllvm-args=-enable-dfa-jump-thread" cargo +nightly build --examples --release
cp "target/release/examples/blogpost-uncompress" "target/release/enable-dfa-jump-thread"

cargo +nightly build --examples --release --features="loop-match"
cp "target/release/examples/blogpost-uncompress" "target/release/loop-match"

RUSTFLAGS="-Cllvm-args=-enable-dfa-jump-thread" cargo +nightly build --examples --release --features="loop-match"
cp "target/release/examples/blogpost-uncompress" "target/release/loop-match-enable-dfa-jump-thread"

# Alternatively "hyperfine"
N=4
poop "./target/release/nightly rs-chunked $N"  "./target/release/enable-dfa-jump-thread rs-chunked $N" "./target/release/loop-match rs-chunked $N" "./target/release/loop-match-enable-dfa-jump-thread rs-chunked $N" "./target/release/loop-match-enable-dfa-jump-thread ng-chunked $N" 

