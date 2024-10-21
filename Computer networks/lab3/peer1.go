package main

import (
    "encoding/json"
    "fmt"
    "log"
    "net"
    "time"

    "net/http"
    "github.com/gorilla/websocket"
)

type Message struct {
    Sender    string `json:"sender"`
    Receiver  string `json:"receiver"`
    Message   string `json:"message"`
    Timestamp string `json:"timestamp"`
}

var (
    myIP = "185.102.139.161"
    myPort = "55420"
    nextPeerIP = "185.102.139.168"
    nextPeerPort = "55421"
)

func main() {
    log.Printf("Starting peer at %s:%s, next peer: %s:%s\n", myIP, myPort, nextPeerIP, nextPeerPort)

    // Start the server to receive messages
    go startPOSTServer()
    go startServer(myIP, myPort)
    go startWebSocketServer()

    // The loop for sending messages is placed in a separate goroutine
    go handleUserInput()
    // The main goroutine will wait for other goroutines to finish (or will be used for monitoring the state)
    select {} // This blocks the main thread and allows goroutines to continue working
}

func handleUserInput() {
    for {
        time.Sleep(5 * time.Second) // Delay before the next attempt
        var input string
        fmt.Printf("Enter a message: ")
        fmt.Scanln(&input)

        // Send a message to the next peer
        message := Message{
            Sender:    myIP + ":" + myPort,
            Receiver:  nextPeerIP + ":" + nextPeerPort,
            Message:   input,
            Timestamp: time.Now().Format(time.RFC3339),
        }

        // Check peer availability before sending
        if isPeerAvailable(nextPeerIP, nextPeerPort) {
            err := sendMessage(nextPeerIP + ":" + nextPeerPort, message)
            if err != nil {
                log.Println("Error sending message:", err)
            }
        } else {
            log.Println("Next peer is unavailable. Trying again later.")
        }
    }
}

// Function to start the server to receive messages
func startServer(ip string, port string) {
    listener, err := net.Listen("tcp", ip+":"+port)
    if err != nil {
        log.Fatal("Error starting server:", err)
    }
    defer listener.Close()

    log.Println("Server started at", ip+":"+port)
    updatePeerState(ip + ":" + port, true)

    for {
        conn, err := listener.Accept()
        if err != nil {
            log.Println("Error accepting client connection:", err)
            continue
        }

        go handleConnection(conn)
    }
}

// Handling incoming connections
func handleConnection(conn net.Conn) {
    defer conn.Close()

    buffer := make([]byte, 4096)
    n, err := conn.Read(buffer)
    if err != nil {
        //log.Println("Error reading data:", err)
        return
    }

    var message Message
    err = json.Unmarshal(buffer[:n], &message)
    if err != nil {
        //log.Println("Error parsing message:", err)
        return
    }
    updatePeerState(message.Sender, true)

    log.Printf("Received message from %s: %s\n", message.Sender, message.Message)
    sendNewMessage(message)
    fmt.Printf("Enter a message: ")
}

// Send a message to the next peer
func sendMessage(nextAddress string, message Message) error {
    conn, err := net.Dial("tcp", nextAddress)
    if err != nil {
        log.Println("Error connecting to the next peer:", err)
        return err
    }
    defer conn.Close()

    jsonMessage, _ := json.Marshal(message)
    _, err = conn.Write(jsonMessage)
    if err != nil {
        log.Println("Error sending message:", err)
        return err
    }

    log.Println("Message sent:", message)
    sendNewMessage(message)
    return nil
}

// Check peer availability using ping
func isPeerAvailable(peerIP, peerPort string) bool {
    var address string
    if peerIP == "localhost" {
        // Check the local port
        address = "localhost:" + peerPort
    } else {
        // Check external IP and port
        address = peerIP + ":" + peerPort
    }

    // Use net.DialTimeout to check peer availability with a timeout
    conn, err := net.DialTimeout("tcp", address, 5*time.Second)
    if err != nil {
        log.Printf("Peer %s is unavailable: %v", address, err)
        //notifyPeerStatus(peerIP, "disconnected")
        return false
    }
    defer conn.Close()
    //notifyPeerStatus(peerIP, "connected")
    //log.Printf("Peer %s is available", address)
    updatePeerState(peerIP + ":" + peerPort, true)
    return true
}

/////////////////////////////////////////   WebSocket   /////////////////////////////////////////
type PeerState struct {
    IP        string `json:"ip"`
    Port      string `json:"port"`
    Connected bool   `json:"connected"`
}

type DashboardMessage struct {
    EventType string    `json:"event_type"` // For example, "state_update" or "new_message"
    PeerState PeerState `json:"peer_state,omitempty"`
    Message   Message   `json:"message,omitempty"`
}

var peers = map[string]PeerState{
    "185.102.139.161:55420": {IP: "185.102.139.161", Port: "55420", Connected: false},
    "185.102.139.168:55421": {IP: "185.102.139.168", Port: "55421", Connected: false},
    "185.102.139.169:55422": {IP: "185.102.139.169", Port: "55422", Connected: false},
}

var clients = make(map[*websocket.Conn]bool)
var broadcast = make(chan DashboardMessage)

var upgrader = websocket.Upgrader{
    CheckOrigin: func(r *http.Request) bool {
        return true
    },
}

// Start the WebSocket server on each peer
func startWebSocketServer() {
    //http.HandleFunc("/", handleRoot) // Handler for the root route
    http.HandleFunc("/ws", handleConnections)
    go handleMessages()
    log.Printf("WebSocket started on 55430\n")
    err := http.ListenAndServe(":55430", nil)
    if err != nil {
        log.Fatalf("Failed to start WebSocket: %v", err)
    }
}

// Handle new WebSocket connections
func handleConnections(w http.ResponseWriter, r *http.Request) {
    ws, err := upgrader.Upgrade(w, r, nil)
    if err != nil {
        log.Println("Error connecting to WebSocket:", err)
        return
    }
    defer ws.Close()

    clients[ws] = true

    // Initial send of all peer states
    for _, peer := range peers {
        message := DashboardMessage{
            EventType: "state_update",
            PeerState: peer,
        }
        ws.WriteJSON(message)
    }

    // Wait for connection termination
    for {
        _, _, err := ws.ReadMessage()
        if err != nil {
            log.Println("WebSocket client disconnected:", err)
            delete(clients, ws)
            break
        }
    }
}

// Send messages to all connected WebSocket clients
func handleMessages() {
    for {
        msg := <-broadcast
        for client := range clients {
            err := client.WriteJSON(msg)
            if err != nil {
                log.Println("Error sending message to WebSocket client:", err)
                client.Close()
                delete(clients, client)
            }
        }
    }
}

// Function to update peer states
func updatePeerState(address string, connected bool) {
    peer := peers[address]
    peer.Connected = connected
    peers[address] = peer

    broadcast <- DashboardMessage{
        EventType: "state_update",
        PeerState: peer,
    }
}

// Example of sending a new message
func sendNewMessage(message Message) {
    broadcast <- DashboardMessage{
        EventType: "new_message",
        Message:   message,
    }
}

/////////////////////////////////////////   POST   /////////////////////////////////////////
func startPOSTServer() {
    http.HandleFunc("/", serveHTML)
    http.HandleFunc("/send", handlePostRequest)
    log.Printf("HTTP server started at http://185.102.139.161:55440")
    log.Fatal(http.ListenAndServe(":55440", nil))
}

func serveHTML(w http.ResponseWriter, r *http.Request) {
    http.ServeFile(w, r, "index.html") // Assuming index.html is in the same directory
}

// Handling POST requests
func handlePostRequest(w http.ResponseWriter, r *http.Request) {
    var reqBody struct {
        Message string `json:"message"`
    }
    err := json.NewDecoder(r.Body).Decode(&reqBody)
    if err != nil {
        http.Error(w, "Invalid request body", http.StatusBadRequest)
        return
    }

    // Create a message for the next peer
    message := Message{
        Sender:    myIP + ":" + myPort,
        Receiver:  nextPeerIP + ":" + nextPeerPort,
        Message:   reqBody.Message,
        Timestamp: time.Now().Format(time.RFC3339),
    }

    // Send the message to the next peer
    err = sendMessage(nextPeerIP + ":" + nextPeerPort, message)
    if err != nil {
        http.Error(w, "Error sending message", http.StatusInternalServerError)
        return
    }

    w.Write([]byte("Message sent successfully"))
}