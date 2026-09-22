package main

import "fmt"

type Rng struct{ state uint64 }

func (r *Rng) next() uint64 {
	x := r.state
	x ^= x << 13
	x ^= x >> 7
	x ^= x << 17
	r.state = x
	return x
}

func main() {
	const n = 5_000_000
	m := make(map[uint64]uint64)
	keys := Rng{0x9E3779B97F4A7C15}
	for i := uint64(0); i < n; i++ {
		m[keys.next()] = i
	}
	hits := Rng{0x9E3779B97F4A7C15}
	misses := Rng{0x2545F4914F6CDD1D}
	var found, sum uint64
	for i := uint64(0); i < n; i++ {
		hitKey := hits.next()
		missKey := misses.next()
		key := missKey
		if i%2 == 0 {
			key = hitKey
		}
		if v, ok := m[key]; ok {
			found++
			sum += v
		}
	}
	fmt.Printf("inserted: %d found: %d sum: %d\n", len(m), found, sum)
}
