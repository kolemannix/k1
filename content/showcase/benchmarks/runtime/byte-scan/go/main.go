package main

import (
	"bytes"
	"encoding/binary"
	"fmt"
)

const (
	size          = 256 * 1024 * 1024
	repeat        = 8
	newlineStride = 4093
	delimStride   = 1000003
)

type Rng struct{ state uint64 }

func (r *Rng) next() uint64 {
	x := r.state
	x ^= x << 13
	x ^= x >> 7
	x ^= x << 17
	r.state = x
	return x
}

func fill(buf []byte) {
	rng := Rng{0x2545F4914F6CDD1D}
	for i := 0; i+8 <= len(buf); i += 8 {
		binary.LittleEndian.PutUint64(buf[i:], (rng.next()&0x1f1f1f1f1f1f1f1f)|0x4040404040404040)
	}
	for i := newlineStride; i < len(buf); i += newlineStride {
		buf[i] = '\n'
	}
	marks := [3]byte{',', ':', '"'}
	which := 0
	for d := delimStride; d < len(buf); d += delimStride {
		buf[d] = marks[which]
		which = (which + 1) % 3
	}
}

func countNewlines(data []byte) int64 {
	var count int64
	from := 0
	for {
		p := bytes.IndexByte(data[from:], '\n')
		if p < 0 {
			return count
		}
		count++
		from += p + 1
	}
}

func scanDelims(data []byte) (found, indexSum, whichSum int64) {
	from := 0
	for {
		p := bytes.IndexAny(data[from:], ",:\"")
		if p < 0 {
			return
		}
		index := from + p
		found++
		indexSum += int64(index)
		switch data[index] {
		case ',':
		case ':':
			whichSum += 1
		default:
			whichSum += 2
		}
		from = index + 1
	}
}

func main() {
	buf := make([]byte, size)
	fill(buf)
	var absent, newlines, delims, indexSum, whichSum int64
	for r := 0; r < repeat; r++ {
		if bytes.IndexByte(buf, 0) >= 0 {
			absent++
		}
		newlines += countNewlines(buf)
		f, i, w := scanDelims(buf)
		delims += f
		indexSum += i
		whichSum += w
	}
	fmt.Printf("absent: %d newlines: %d delims: %d index-sum: %d which-sum: %d\n", absent, newlines, delims, indexSum, whichSum)
}
