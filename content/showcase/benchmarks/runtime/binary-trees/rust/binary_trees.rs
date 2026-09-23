struct Node {
    left: Option<Box<Node>>,
    right: Option<Box<Node>>,
}

fn make(depth: u32) -> Box<Node> {
    if depth == 0 {
        Box::new(Node { left: None, right: None })
    } else {
        Box::new(Node { left: Some(make(depth - 1)), right: Some(make(depth - 1)) })
    }
}

fn check(n: &Node) -> i64 {
    match (&n.left, &n.right) {
        (Some(l), Some(r)) => 1 + check(l) + check(r),
        _ => 1,
    }
}

fn main() {
    let max_depth: u32 = 20;
    let stretch = max_depth + 1;
    println!("stretch tree of depth {}\t check: {}", stretch, check(&make(stretch)));
    let long_lived = make(max_depth);
    let mut depth = 4;
    while depth <= max_depth {
        let iterations = 1i64 << (max_depth - depth + 4);
        let mut total = 0i64;
        for _ in 0..iterations {
            total += check(&make(depth));
        }
        println!("{}\t trees of depth {}\t check: {}", iterations, depth, total);
        depth += 2;
    }
    println!("long lived tree of depth {}\t check: {}", max_depth, check(&long_lived));
}
