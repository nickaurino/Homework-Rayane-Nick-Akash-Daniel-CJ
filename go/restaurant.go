package main

import (
	"log"
	"math/rand"
	"sync"
	"sync/atomic"
	"time"
)

type Order struct {
	id        uint64
	customer  string
	reply     chan *Order
	preparedBy string
}

var orderID atomic.Uint64

func randomDuration(minMillis, maxMillis int) {
	time.Sleep(time.Millisecond * time.Duration(minMillis+rand.Intn(maxMillis-minMillis)))
}

func cook(name string, waiter chan *Order) {
	log.Println(name, "starting work")
	for {
		order, ok := <-waiter
		if !ok {
			log.Println(name, "closing down")
			return
		}
		log.Println(name, "cooking order", order.id, "for", order.customer)
		randomDuration(5000, 10000)
		order.preparedBy = name
		order.reply <- order
	}
}

func customer(name string, waiter chan *Order, wg *sync.WaitGroup) {
	defer wg.Done()
	mealsEaten := 0
	for mealsEaten < 5 {
		order := &Order{
			id:        orderID.Add(1),
			customer:  name,
			reply:     make(chan *Order, 1),
		}
		log.Println(name, "placed order", order.id)

		select {
		case waiter <- order:
			meal := <-order.reply
			log.Println(name, "eating cooked order", meal.id, "prepared by", meal.preparedBy)
			randomDuration(1000, 2000)
			mealsEaten++
		case <-time.After(7 * time.Second):
			log.Println(name, "waiting too long, abandoning order", order.id)
			randomDuration(2500, 5000)
		}
	}
	log.Println(name, "going home")
}

func main() {
	rand.Seed(time.Now().UnixNano())

	waiter := make(chan *Order, 3)
	var wg sync.WaitGroup

	cooks := []string{"Remy", "Colette", "Linguini"}
	for _, name := range cooks {
		go cook(name, waiter)
	}

	customers := []string{"Ani", "Bai", "Cat", "Dao", "Eve", "Fay", "Gus", "Hua", "Iza", "Jai"}
	wg.Add(len(customers))
	for _, name := range customers {
		go customer(name, waiter, &wg)
	}

	wg.Wait()

	close(waiter)
	log.Println("Restaurant closing")
}
