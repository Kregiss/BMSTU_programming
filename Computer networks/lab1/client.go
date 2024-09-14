package main

import (
        "encoding/json"
        "flag"
        "fmt"
        "github.com/skorobogatov/input"
        "net"
)

import "lab1/src/proto"

func interact(conn *net.TCPConn) {
        defer conn.Close()
        encoder, decoder := json.NewEncoder(conn), json.NewDecoder(conn)
        for {
                fmt.Printf("command = ")
                command := input.Gets()

                switch command {
                case "quit":
                        send_request(encoder, "quit", nil)
                        return
                case "add":
                        fmt.Printf("degree = ")
                        var degree int
                        fmt.Scan(&degree)

                        coefficients := make([]float64, degree + 1)
                        fmt.Printf("coefficients = ")
                        for i := 0; i <= degree; i++ {
                                fmt.Scan(&coefficients[i])
                        }

                        fmt.Printf("point = ")
                        var point float64
                        fmt.Scan(&point)

                        polynomial := proto.Polynomial{
                                Degree: int(degree),
                                Coefficients: coefficients,
                                X: point,
                        }
                        send_request(encoder, "add", &polynomial)
                case "avg":
                        send_request(encoder, "avg", nil)
        default:
            fmt.Printf("error: unknown command\n")
            continue
                }

                var resp proto.Response
                if err := decoder.Decode(&resp); err != nil {
                        fmt.Printf("error: %v\n", err)
                        break
                }

                switch resp.Status {
                case "ok":
                        fmt.Printf("ok\n")
                case "failed":
                        if resp.Data == nil {
                                fmt.Printf("error: data field is absent in response\n")
                        } else {
                                var errorMsg string
                                if err := json.Unmarshal(*resp.Data, &errorMsg); err != nil {
                                        fmt.Printf("error: malformed data field in response\n")
                                } else {
                                        fmt.Printf("failed: %s\n", errorMsg)
                                }
                        }
                case "result":
                        if resp.Data == nil {
                                fmt.Printf("error: data field is absent in response\n")
                        } else {
                                var result float64
                                if err := json.Unmarshal(*resp.Data, &result); err != nil {
                                        fmt.Printf("error: malformed data field in response\n")
                                } else {
                                        fmt.Printf("Polynomial result: %f\n", result)
                                }
                        }
                default:
                        fmt.Printf("error: server reports unknown status %q\n", resp.Status)
                }
        }
}

func send_request(encoder *json.Encoder, command string, data interface{}) {
        var raw json.RawMessage
        raw, _ = json.Marshal(data)
        encoder.Encode(&proto.Request{command, &raw})
}

func main() {
        var addrStr string
        flag.StringVar(&addrStr, "addr", "185.102.139.168:5451", "specify ip address and port")
        flag.Parse()

        if addr, err := net.ResolveTCPAddr("tcp", addrStr); err != nil {
                fmt.Printf("error: %v\n", err)
        } else if conn, err := net.DialTCP("tcp", nil, addr); err != nil {
                fmt.Printf("error: %v\n", err)
        } else {
                interact(conn)
        }
}
