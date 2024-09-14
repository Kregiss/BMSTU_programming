package main

import (
        "encoding/json"
        "flag"
        "fmt"
        "github.com/mgutz/logxi/v1"
        "math"
        "net"
)

import "lab1/src/proto"

type Client struct {
        logger log.Logger   
        conn   *net.TCPConn  
        enc    *json.Encoder
        polynomials []proto.Polynomial
}

func NewClient(conn *net.TCPConn) *Client {
        return &Client{
                logger: log.New(fmt.Sprintf("client %s", conn.RemoteAddr().String())),
                conn:   conn,
                enc:    json.NewEncoder(conn),
                polynomials: []proto.Polynomial{},
        }
}

func (client *Client) serve() {
        defer client.conn.Close()
        decoder := json.NewDecoder(client.conn)
        for {
                var req proto.Request
                if err := decoder.Decode(&req); err != nil {
                        client.logger.Error("cannot decode message", "reason", err)
                        break
                } else {
                        client.logger.Info("received command", "command", req.Command)
                        if client.handleRequest(&req) {
                                client.logger.Info("shutting down connection")
                                break
                        }
                }
        }
}

func (client *Client) handleRequest(req *proto.Request) bool {
        switch req.Command {
        case "quit":
                client.respond("ok", nil)
                return true
        case "add":
                errorMsg := ""
                if req.Data == nil {
                        errorMsg = "data field is absent"
                } else {
                        var polynomial proto.Polynomial
                        if err := json.Unmarshal(*req.Data, &polynomial); err != nil {
                                errorMsg = "malformed data field"
                        } else {
                                client.logger.Info("adding polynomial", "polynomial", polynomial)
                                client.polynomials = append(client.polynomials, polynomial)
                        }
                }
                if errorMsg == "" {
                        client.respond("ok", nil)
                } else {
                        client.logger.Error("addition failed", "reason", errorMsg)
                        client.respond("failed", errorMsg)
                }
        case "avg":
                if len(client.polynomials) == 0 {
                        client.logger.Error("calculation failed", "reason", "no polynomials added")
                        client.respond("failed", "no polynomials added")
                } else {
                        lastPolynomial := client.polynomials[len(client.polynomials)-1]
                        result := evaluatePolynomial(lastPolynomial)
                        client.respond("result", result)
                }
        default:
                client.logger.Error("unknown command")
                client.respond("failed", "unknown command")
        }
        return false
}

func evaluatePolynomial(polynomial proto.Polynomial) float64 {
        result := 0.0
        for i := 0; i <= polynomial.Degree; i++ {
                result += polynomial.Coefficients[i] * math.Pow(polynomial.X, float64(i))
        }
        return result
}

func (client *Client) respond(status string, data interface{}) {
        var raw json.RawMessage
        raw, _ = json.Marshal(data)
        client.enc.Encode(&proto.Response{status, &raw})
}

func main() {
        var addrStr string
        flag.StringVar(&addrStr, "addr", "185.102.139.168:5451", "specify ip address and port")
        flag.Parse()

        if addr, err := net.ResolveTCPAddr("tcp", addrStr); err != nil {
                log.Error("address resolution failed", "address", addrStr)
        } else {
                log.Info("resolved TCP address", "address", addr.String())

                if listener, err := net.ListenTCP("tcp", addr); err != nil {
                        log.Error("listening failed", "reason", err)
                } else {
                        for {
                                if conn, err := listener.AcceptTCP(); err != nil {
                                        log.Error("cannot accept connection", "reason", err)
                                } else {
                                        log.Info("accepted connection", "address", conn.RemoteAddr().String())
                                        go NewClient(conn).serve()
                                }
                        }
                }
        }
}
