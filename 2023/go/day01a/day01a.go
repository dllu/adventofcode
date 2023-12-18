package main

import (
	"fmt"
	"log"
	"os"
	"strings"
)

func usage(progname string) {
	log.Fatalf("usage: %s <file>\n", progname)
	os.Exit(1)
}

func process(contents string) int {
	sum := 0
	digits := [...]string{"0", "1", "2", "3", "4", "5", "6", "7", "8", "9"}
	lines := strings.Split(contents, "\n")
	for _, line := range lines {
		minIndex := len(line)
		maxIndex := -1
		leftDigit := 0
		rightDigit := 0
		for i, digit_str := range digits {
			leftIndex := strings.Index(line, digit_str)
			if leftIndex != -1 && leftIndex < minIndex {
				minIndex = leftIndex
				leftDigit = i
			}
			rightIndex := strings.LastIndex(line, digit_str)
			if rightIndex != -1 && rightIndex > maxIndex {
				maxIndex = rightIndex
				rightDigit = i
			}
		}
		sum += leftDigit*10 + rightDigit
	}
	return sum
}

func main() {
	if len(os.Args) < 2 {
		usage(os.Args[0])
	}
	filename := os.Args[1]
	buf, err := os.ReadFile(filename)
	if err != nil {
		log.Fatal(err)
	}
	contents := string(buf[:])
	result := process(contents)
	fmt.Printf("result = %d\n", result)
}
