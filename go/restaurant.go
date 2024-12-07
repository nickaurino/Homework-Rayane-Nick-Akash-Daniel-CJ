package main

import (
	"log"
	"math/rand"
	"sync"
	"sync/atomic"
	"time"
)

// Order represents a meal order placed by a customer.
type Order struct {
	id        uint64
	customer  string
	reply     chan *Order
	preparedBy string
}

var orderID atomic.Uint64

// randomDuration simulates an action taking a random amount of time within a range.
func randomDuration(minMillis, maxMillis int) {
	time.Sleep(time.Millisecond * time.Duration(minMillis+rand.Intn(maxMillis-minMillis)))
}

// cook simulates a cook who prepares orders taken from the waiter channel.
func cook(name string, waiter chan *Order, done <-chan struct{}) {
	log.Println(name, "starting work")
	for {
		select {
		case order, ok := <-waiter:
			if !ok { 
				log.Println(name, "closing down")
				return
			}
			log.Println(name, "cooking order", order.id, "for", order.customer)
			randomDuration(5000, 10000) // Cooking time.
			order.preparedBy = name
			order.reply <- order 
		case <-done: 
			log.Println(name, "received shutdown signal")
			return
		}
	}
}

// customer simulates a customer placing orders and eating meals.
func customer(name string, waiter chan *Order, wg *sync.WaitGroup) {
	defer wg.Done() 
	mealsEaten := 0
	for mealsEaten < 5 { // Customer eats 5 meals before going home.
		order := &Order{
			id:        orderID.Add(1), 
			customer:  name,
			reply:     make(chan *Order, 1),
		}
		log.Println(name, "placed order", order.id)

		select {
		case waiter <- order: // Successfully placed order with the waiter.
			meal := <-order.reply 
			log.Println(name, "eating cooked order", meal.id, "prepared by", meal.preparedBy)
			randomDuration(1000, 2000) // Time to eat the meal.
			mealsEaten++
		case <-time.After(7 * time.Second): //Abandoning if waiter is too busy.
			log.Println(name, "waiting too long, abandoning order", order.id)
			randomDuration(2500, 5000) // Customer leaves and comes back later.
		}
	}
	log.Println(name, "going home") 
}

// main initializes the simulation, creating cooks and customers, and manages shutdown.
func main() {
	rand.Seed(time.Now().UnixNano()) 

	waiter := make(chan *Order, 3) 
	done := make(chan struct{})    
	var wg sync.WaitGroup         


	cooks := []string{"Remy", "Colette", "Linguini"}
	for _, name := range cooks {
		go cook(name, waiter, done)
	}

	customers := []string{"Ani", "Bai", "Cat", "Dao", "Eve", "Fay", "Gus", "Hua", "Iza", "Jai"}
	wg.Add(len(customers))
	for _, name := range customers {
		go customer(name, waiter, &wg)
	}

	wg.Wait() 

	// Close waiter channel and notify cooks to shut down.
	close(waiter)
	close(done)

	log.Println("Restaurant closing") 
}
