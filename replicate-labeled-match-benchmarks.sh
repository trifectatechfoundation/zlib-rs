cargo clean -p test-libz-rs-sys 

git checkout a32ae851ebdcb92c5f1f442e4efa464132c96b7f
git log -1
RUSTFLAGS="-Ctarget-cpu=native" cargo +stage1 build --release --example blogpost-uncompress
cp target/release/examples/blogpost-uncompress /tmp/uncompress-baseline

git checkout c7bddad2ad4fbb2b41d8a28459b807c710034687
git log -1
RUSTFLAGS="-Ctarget-cpu=native" cargo +stage1 build --release --example blogpost-uncompress
cp target/release/examples/blogpost-uncompress /tmp/loop-plus-match

git checkout 98b83cbd752468d4761829bab6563bc27e64957b
git log -1
RUSTFLAGS="-Ctarget-cpu=native" cargo +stage1 build --release --example blogpost-uncompress
cp target/release/examples/blogpost-uncompress /tmp/labeled-match-len

git checkout len-as-match
git log -1
RUSTFLAGS="-Ctarget-cpu=native" cargo +stage1 build --release --example blogpost-uncompress
cp target/release/examples/blogpost-uncompress /tmp/labeled-match-fast

for i in 4 7 16; do
  poop "/tmp/uncompress-baseline rs-chunked $i silesia-small.tar.gz" \
       "/tmp/loop-plus-match rs-chunked $i silesia-small.tar.gz" \
       "/tmp/labeled-match-len rs-chunked $i silesia-small.tar.gz" \
       "/tmp/labeled-match-fast rs-chunked $i silesia-small.tar.gz"
done
