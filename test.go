// You can edit this code!
// Click here and start typing.
package main

import "fmt"
import "time"

func reader(c chan int, done chan struct{}) {
	for {
		_, ok := <-c
		if ok == false {
			done <- struct{}{}
			break
		}
	}

}
func writer(c chan int, count int) {
	for x := 0; x < count; x++ {
		c <- x

	}
	close(c)

}
func reader_select(c1 chan int,c2 chan int, done chan struct{}) {
	for {
		select{
			case _, ok := <-c1:
			if ok == false {
				done <- struct{}{}
				return
			}

			case _, ok := <-c2:
			if ok == false {
				done <- struct{}{}
				return
			}

		}
	}

}
func writer_select(c1 chan int,c2 chan int, count int) {
	for x := 0; x < count; x++ {
		if x % 2 == 0{
			c1 <- x

		}else{

			c2 <- x
		}

	}
	close(c1)
	close(c2)

}


func main() {
	count := 1000000

	done:= make(chan struct{})
	start := time.Now()

	c := make(chan int)

	go reader(c,done)
	go writer(c,count)
	<- done
	elapsed := time.Since(start)

	fmt.Printf("channel took %s\n", elapsed)

	start = time.Now()

	c1 := make(chan int)
	c2 := make(chan int)

	go reader_select(c1,c2,done)
	go writer_select(c1,c2,count)
	<- done
	elapsed = time.Since(start)

	fmt.Printf("channel select took %s\n", elapsed)
}

