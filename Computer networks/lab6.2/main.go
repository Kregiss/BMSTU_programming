package main

import (
        "fmt"
        "log"
        "net/http"
        "strings"

        "github.com/gorilla/websocket"
        "github.com/jlaffaye/ftp"
)

var (
        ftpClient *ftp.ServerConn
        upgrader  = websocket.Upgrader{}
        clients   = make(map[*websocket.Conn]bool)
)

func main() {
        http.HandleFunc("/ws", handleWebSocket)

        // Статические файлы
        fs := http.FileServer(http.Dir("./html"))
        http.Handle("/", fs)

        log.Println("Server running on http://185.104.251.226:5421")
        log.Fatal(http.ListenAndServe(":5421", nil))
}

// Обработчик WebSocket
func handleWebSocket(w http.ResponseWriter, r *http.Request) {
        upgrader.CheckOrigin = func(r *http.Request) bool { return true }
        conn, err := upgrader.Upgrade(w, r, nil)
        if err != nil {
                log.Println("Error upgrading to WebSocket:", err)
                return
        }
        defer conn.Close()
        clients[conn] = true
        defer delete(clients, conn)

        for {
                var message map[string]string
                if err := conn.ReadJSON(&message); err != nil {
                        log.Println("Error reading message:", err)
                        break
                }

                handleCommand(message, conn)
        }
}

// Обработчик FTP-команд
func handleCommand(msg map[string]string, conn *websocket.Conn) {
        command := msg["command"]
        args := strings.Fields(command)
        switch args[0] {
        case "connect":
                server, user, password := msg["server"], msg["username"], msg["password"]
                if !strings.Contains(server, ":") {
                        server += ":21"
                }

                var err error
                ftpClient, err = ftp.Dial(server)
                if err != nil {
                        sendError(conn, "Failed to connect to FTP server", err.Error())
                        return
                }

                err = ftpClient.Login(user, password)
                if err != nil {
                        sendError(conn, "Failed to log in to FTP server", err.Error())
                        return
                }

                sendMessage(conn, "Connected to FTP server")

        case "ls":
                if ftpClient == nil {
                        sendError(conn, "Not connected to any FTP server", "")
                        return
                }

                entries, err := ftpClient.List("")
                if err != nil {
                        sendError(conn, "Failed to list directory", err.Error())
                        return
                }

                files := []map[string]string{}
                for _, entry := range entries {
                        files = append(files, map[string]string{
                                "name": entry.Name,
                                "type": func() string {
                                        if entry.Type == ftp.EntryTypeFile {
                                                return "file"
                                        }
                                        return "directory"
                                }(),
                        })
                }


                conn.WriteJSON(map[string]interface{}{
                        "success": true,
                        "files":   files,
                })
        case "rm":
                if len(args) < 2 {
                        conn.WriteJSON(map[string]interface{}{"success": false, "message": "Usage: rm <file>"})
                        return
                }
                err := ftpClient.Delete(args[1])
                if err != nil {
                        conn.WriteJSON(map[string]interface{}{"success": false, "message": "Failed to remove file", "error": err.Error()})
                        return
                }
                sendDirectoryListing(conn, ftpClient, "File removed")
        case "mkdir":
                if len(args) < 2 {
                        conn.WriteJSON(map[string]interface{}{"success": false, "message": "Usage: mkdir <directory>"})
                        return
                }
                err := ftpClient.MakeDir(args[1])
                if err != nil {
                        conn.WriteJSON(map[string]interface{}{"success": false, "message": "Failed to create directory", "error": err.Error()})
                        return
                }
                sendDirectoryListing(conn, ftpClient, "Directory created")
        case "rmdir":
                if len(args) < 2 {
                        conn.WriteJSON(map[string]interface{}{"success": false, "message": "Usage: rmdir <directory>"})
                        return
                }
                err := ftpClient.RemoveDir(args[1])
                if err != nil {
                        conn.WriteJSON(map[string]interface{}{"success": false, "message": "Failed to remove directory", "error": err.Error()})
                        return
                }
                sendDirectoryListing(conn, ftpClient, "Directory removed")
        default:
                sendError(conn, "Unknown command", "")
        }
}

// Общая функция для отправки содержимого директории
func sendDirectoryListing(conn *websocket.Conn, ftpClient *ftp.ServerConn, message string) {
        entries, err := ftpClient.List("")
                if err != nil {
                        sendError(conn, "Failed to list directory", err.Error())
                        return
                }

                files := []map[string]string{}
                for _, entry := range entries {
                        files = append(files, map[string]string{
                                "name": entry.Name,
                                "type": func() string {
                                        if entry.Type == ftp.EntryTypeFile {
                                                return "file"
                                        }
                                        return "directory"
                                }(),
                        })
                }

                fmt.Println(message)

                conn.WriteJSON(map[string]interface{}{
                        "success": true,
                        "files":   files,
                })
}

func sendError(conn *websocket.Conn, message, details string) {
        conn.WriteJSON(map[string]interface{}{
                "success": false,
                "message": message,
                "details": details,
        })
}

func sendMessage(conn *websocket.Conn, message string) {
        conn.WriteJSON(map[string]interface{}{
                "success": true,
                "message": message,
        })
}



