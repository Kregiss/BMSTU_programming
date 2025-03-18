package main

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"log"
	"math/big"
	"net/http"
	"time"

	"github.com/ethereum/go-ethereum/ethclient"
)

type BlockData struct {
	Number       		uint64 					`json:"number"`
	Time         		uint64 					`json:"time"`
	Difficulty   		uint64 					`json:"difficulty"`
	Hash         		string 					`json:"hash"`
	Transactions_count 	int 					`json:"transactions count"`
	Transactions 		[]TransactionData    	`json:"transactions"`
}

type TransactionData struct {
	Hash      string `json:"hash"`
	To        string `json:"to"`
	Value     string `json:"value"`
	Gas       uint64 `json:"gas"`
	GasPrice  string `json:"gasPrice"`
}

func main() {
	client, err := ethclient.Dial("https://mainnet.infura.io/v3/d1eccb6a8c334a47b8e6635379bb4fe8")
	if err != nil {
		log.Fatalf("Ошибка подключения к Ethereum: %v", err)
	}

	firebaseURL := "https://lab7-f46db-default-rtdb.europe-west1.firebasedatabase.app/blocks/latest.json"
	apiKey := "AIzaSyDnboKcMq5mhnBQebKrxhFuyI1Ilh0aM9M" 

	var latestBlock uint64

	for {
		header, err := client.HeaderByNumber(context.Background(), nil)
		if err != nil {
			log.Fatalf("Ошибка получения последнего блока: %v", err)
			time.Sleep(5 * time.Second)
			continue
		}

		currentBlock := header.Number.Uint64()
		if currentBlock > latestBlock {
			latestBlock = currentBlock

			blockNumber := big.NewInt(header.Number.Int64())
			block, err := client.BlockByNumber(context.Background(), blockNumber)
			if err != nil {
				log.Fatalf("Ошибка получения данных блока: %v", err)
			}

			var transactions []TransactionData
			for _, tx := range block.Transactions() {
				toAddress := ""
				if tx.To() != nil {
					toAddress = tx.To().Hex()
				}

				transactions = append(transactions, TransactionData{
					Hash:     tx.Hash().Hex(),
					To:       toAddress,
					Value:    tx.Value().String(),
					Gas:      tx.Gas(),
					GasPrice: tx.GasPrice().String(),
				})
			}

			blockData := BlockData{
				Number:       		block.Number().Uint64(),
				Time:         		block.Time(),
				Difficulty:   		block.Difficulty().Uint64(),
				Hash:         		block.Hash().Hex(),
				Transactions_count: len(block.Transactions()),
				Transactions: 		transactions,
			}

			fmt.Printf("Полученные данные блока: %+v\n", blockData.Number)
			
			data, err := json.Marshal(blockData)
			if err != nil {
				log.Fatalf("Ошибка преобразования данных в JSON: %v", err)
			}

			req, err := http.NewRequestWithContext(context.Background(), "POST",  firebaseURL+"?auth="+apiKey, bytes.NewBuffer(data))
			if err != nil {
				log.Fatalf("Ошибка создания HTTP-запроса: %v", err)
			}

			req.Header.Set("Content-Type", "application/json")
			clientHTTP := &http.Client{}
			resp, err := clientHTTP.Do(req)
			if err != nil {
				log.Fatalf("Ошибка отправки данных в Firebase: %v", err)
				time.Sleep(5 * time.Second)
				continue
			}
			defer resp.Body.Close()

			if resp.StatusCode == http.StatusOK {
				fmt.Println("Данные успешно записаны в Firebase!")
			} else {
				log.Fatalf("Ошибка записи данных в Firebase: %v", resp.Status)
			}
		}
		time.Sleep(5 * time.Second)
	}
}
