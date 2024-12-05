package main

import (
	"log"
	"math/rand"
	"sync"
	"sync/atomic"
	"time"
)

// Logging utility
func logMessage(args ...any) {
	log.Println(args...)
}

// Simulate performing a task for a random duration
func doAction(seconds int, action ...any) {
	logMessage(action...)
	randomMillis := 5000 + rand.Intn(5000)
	time.Sleep(time.Duration(randomMillis) * time.Millisecond)
}

// Order structure with unique ID and a response mechanism
type Order struct {
	id         uint64
	customer   string
	preparedBy string
	reply      chan *Order // Acts like CompletableFuture
}

var nextId atomic.Uint64
var waiter = make(chan *Order, 3) // Waiter can handle up to 3 orders

// Cook logic
func cook(name string, wg *sync.WaitGroup) {
	defer wg.Done() // Ensure cook goroutines are accounted for

	logMessage(name, "starting work")
	for order := range waiter { // Continuously pick up orders
		doAction(1, name, "is cooking order", order.id, "for", order.customer)
		order.preparedBy = name
		order.reply <- order // Send the cooked order back
	}
}

// Customer logic
func customer(name string, wg *sync.WaitGroup) {
	defer wg.Done() // Mark this customer as done when the function exits
	mealsEaten := 0

	for mealsEaten < 5 {
		order := &Order{
			id:       nextId.Add(1),
			customer: name,
			reply:    make(chan *Order), // Use unbuffered channel for proper synchronization
		}

		logMessage(name, "placed order", order.id)

		// Try to place the order with the waiter
		select {
		case waiter <- order: // Successfully placed the order
			select {
			case completedOrder := <-order.reply: // Successfully got the meal
				doAction(2, name, "eating cooked order", completedOrder.id, "prepared by", completedOrder.preparedBy)
				mealsEaten++
			case <-time.After(7 * time.Second): // Timeout while waiting for the cook
				logMessage(name, "waiting too long, abandoning order", order.id)
			}
		case <-time.After(7 * time.Second): // Timeout trying to place order
			logMessage(name, "could not place the order due to a busy waiter")
		}

		// Wait before trying again if not done eating
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

	// Start cook goroutines
	cookNames := []string{"Remy", "Colette", "Linguini"}
	for _, name := range cookNames {
		wg.Add(1)
		go cook(name, &wg)
	}

	// Start customer goroutines
	for _, customerName := range customers {
		wg.Add(1)
		go customer(customerName, &wg)
	}

	// Wait for all customers to finish
	wg.Wait()
	close(waiter) // Close waiter channel when all customers are done

	logMessage("Restaurant closing")
}
