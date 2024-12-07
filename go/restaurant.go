package main

import (
	"log"
	"math/rand"
	"sync"
	"sync/atomic"
	"time"
)

func logMessage(args ...any) {
	log.Println(args...)
}


func doAction(seconds int, action ...any) {
	logMessage(action...)
	randomMillis := 5000 + rand.Intn(5000)
	time.Sleep(time.Duration(randomMillis) * time.Millisecond)
}


type Order struct {
	id         uint64
	customer   string
	preparedBy string
	reply      chan *Order 
}

var nextId atomic.Uint64
var waiter = make(chan *Order, 3) 


func cook(name string, wg *sync.WaitGroup) {
	defer wg.Done() 

	logMessage(name, "starting work")
	for order := range waiter { 
		doAction(1, name, "is cooking order", order.id, "for", order.customer)
		order.preparedBy = name
		order.reply <- order 
	}
}


func customer(name string, wg *sync.WaitGroup) {
	defer wg.Done() 
	mealsEaten := 0

	for mealsEaten < 5 {
		order := &Order{
			id:       nextId.Add(1),
			customer: name,
			reply:    make(chan *Order), 
		}

		logMessage(name, "placed order", order.id)


		select {
		case waiter <- order: 
			select {
			case completedOrder := <-order.reply: 
				doAction(2, name, "eating cooked order", completedOrder.id, "prepared by", completedOrder.preparedBy)
				mealsEaten++
			case <-time.After(7 * time.Second):
				logMessage(name, "waiting too long, abandoning order", order.id)
			}
		case <-time.After(7 * time.Second): 
			logMessage(name, "could not place the order due to a busy waiter")
		}


		if mealsEaten < 5 {
			waitTime := 2500 + rand.Intn(2500)
			time.Sleep(time.Duration(waitTime) * time.Millisecond)
		}
	}

	logMessage(name, "has eaten 5 meals and is going home")
}

func main() {
	rand.Seed(time.Now().UnixNano())

	customers := []string{
		"Ani", "Bai", "Cat", "Dao", "Eve", "Fay", "Gus", "Hua", "Iza", "Jai",
	}

	var wg sync.WaitGroup


	cookNames := []string{"Remy", "Colette", "Linguini"}
	for _, name := range cookNames {
		wg.Add(1)
		go cook(name, &wg)
	}


	for _, customerName := range customers {
		wg.Add(1)
		go customer(customerName, &wg)
	}


	wg.Wait()
	close(waiter) 

	logMessage("Restaurant closing")
}
