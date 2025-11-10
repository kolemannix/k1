Been working on cross builds, notes

cargo cross uses docker buildx, great

# Build for linux x86-64
```sh
cross build --release --target=x86_64-unknown-linux-gnu --features=llvm-sys/force-static
```

# Build my test image for seeing if the linux build works
docker buildx build --platform linux/amd64 -f builds/Dockerfile-test-linux-amd64 --load -t k1-linux-amd64:latest .

# Run test image
```sh
docker run -t -d --name k1-linux-amd64 --platform linux/amd64 -v ./target/x86_64-unknown-linux-gnu/release/compiler:/root/k1/k1 -v ./stdlib/:/root/k1/stdlib -v ./rt/:/root/k1/rt k1-linux-amd64:latest
docker exec -it linux-amd64 bash
export LLVM_SYS_180_PREFIX=/usr/lib/llvm-18
cd root/k1
```

llvm-sys features:
- Choose how to link llvm; useful for faster builds; should be static for actual builds
inkwell features:
- no-link-libffi in case your llvm doesnt have it configured
