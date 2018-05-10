package main

import (
	"encoding/json"
	"io/ioutil"
)

var (
	ItemsList []Item
)

func LoadItems() {
	file, err := ioutil.ReadFile("items.json")
	if err != nil {
		panic(err)
	}
	err = json.Unmarshal(file, &ItemsList)
	if err != nil {
		panic(err)
	}
}

func init() {
	ItemsList = make([]Item, 0)
	LoadItems()

}

// Save DB data to json file
func saveDB() {

}
