package main

import (
	"bufio"
	"bytes"
	"encoding/json"
	"io"
	"io/ioutil"
	"log"
	"net/http"
	"net/url"
	"os/exec"
	"strconv"
	"time"
)

const siaPort int = 5000

var defaultHTTPTimeout = 240 * time.Second

var client *http.Client = &http.Client{Timeout: defaultHTTPTimeout}

func startBandit() {
	cmd := exec.Command("python bandit_server.py", "-items", len(ItemsList), "-positions", 2, "-contexts", 3)
	cmd.Dir = "/usr/local/Code/src/elysia/server"
	output, err := cmd.CombinedOutput()
	if err != nil {
		panic(err)
	}
	var resp string
	buffer := bufio.NewScanner(bytes.NewReader(output))
	for buffer.Scan() {
		t := buffer.Text()
		resp += "\n" + t
	}
	err = buffer.Err()
	if err != io.EOF && err != nil {
		panic(err)
	}
	log.Println("run response:", resp)
}

func handleClick(w http.ResponseWriter, r *http.Request, user User) {
	// parse context, item, position
	v, _ := json.Marshal(user)
	res, err := client.PostForm("http://localhost:"+strconv.Itoa(siaPort)+"/handle_user_action",
		url.Values{"context": {string(v)}, "item": {r.FormValue("item")}, "position": {r.FormValue("position")}, "reward": {"1"}})
	if err != nil {
		panic(err)
	}
	defer res.Body.Close()
	resBody, err := ioutil.ReadAll(res.Body)
	if err != nil {
		panic(err)
	}
	w.WriteHeader(200)
	w.Write(resBody)
}

func getItems(r *http.Request, user User) Items {
	v, _ := json.Marshal(user)
	res, err := client.PostForm("http://localhost:"+strconv.Itoa(siaPort)+"/get_items",
		url.Values{"context": {string(v)}, "item": {r.FormValue("item")}, "position": {r.FormValue("position")}, "reward": {}})
	if err != nil {
		panic(err)
	}
	defer res.Body.Close()
	resBody, err := ioutil.ReadAll(res.Body)
	if err != nil {
		panic(err)
	}
	itemIdx := []int{}
	err = json.Unmarshal(resBody, &itemIdx)
	if err != nil {
		panic(err)
	}
	items := make([]Item, len(itemIdx))
	for i, idx := range itemIdx {
		items[i] = ItemsList[idx]
	}
	return items
}
