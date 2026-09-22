package main

import "fmt"

type Node struct{ left, right *Node }

func bottomUp(depth int) *Node {
	if depth == 0 {
		return &Node{}
	}
	return &Node{bottomUp(depth - 1), bottomUp(depth - 1)}
}

func (n *Node) check() int {
	if n.left == nil {
		return 1
	}
	return 1 + n.left.check() + n.right.check()
}

func main() {
	maxDepth := 20
	stretch := maxDepth + 1
	fmt.Printf("stretch tree of depth %d\t check: %d\n", stretch, bottomUp(stretch).check())
	longLived := bottomUp(maxDepth)
	for depth := 4; depth <= maxDepth; depth += 2 {
		iterations := 1 << (maxDepth - depth + 4)
		total := 0
		for i := 0; i < iterations; i++ {
			total += bottomUp(depth).check()
		}
		fmt.Printf("%d\t trees of depth %d\t check: %d\n", iterations, depth, total)
	}
	fmt.Printf("long lived tree of depth %d\t check: %d\n", maxDepth, longLived.check())
}
