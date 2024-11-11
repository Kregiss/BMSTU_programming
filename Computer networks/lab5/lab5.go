package main

import (
    "database/sql"
    "encoding/json"
    "fmt"
    "github.com/SlyMarbo/rss"
    "github.com/gorilla/websocket"
    _ "github.com/go-sql-driver/mysql"
    "log"
    "net/http"
    "sync"
    "time"
)

var db *sql.DB
var wsUpgrader = websocket.Upgrader{CheckOrigin: func(r *http.Request) bool { return true }}
var connections = make(map[*websocket.Conn]bool) // Хранилище для WebSocket подключений
var connectionsMutex sync.Mutex                  // Мьютекс для синхронизации доступа к подключению

// Подключение к базе данных MySQL
func initDB() {
    var err error
    db, err = sql.Open("mysql", "iu9networkslabs:Je2dTYr6@tcp(students.yss.su)/iu9networkslabs")
    if err != nil {
        log.Fatal("Ошибка подключения к базе данных:", err)
    }
}

// Проверка, существует ли новость в базе данных
func newsExists(link string) bool {
    var exists bool
    err := db.QueryRow("SELECT EXISTS(SELECT 1 FROM iu9ivenkova WHERE link=?)", link).Scan(&exists)
    if err != nil {
        log.Println("Ошибка при проверке существования новости:", err)
    }
    return exists
}

// Вставка новости в базу данных
func insertNews(item *rss.Item) {
    _, err := db.Exec("INSERT INTO iu9ivenkova (title, link, description, pubDate) VALUES (?, ?, ?, ?)",
        item.Title, item.Link, item.Summary, item.Date)
    if err != nil {
        log.Println("Ошибка при вставке новости:", err)
    }
}

// Обновление базы данных новостями из RSS
func updateNews() {
    rssUrl := "https://www.bragazeta.ru/feed/"
    rssObject, err := rss.Fetch(rssUrl)
    if err != nil {
        log.Println("Ошибка загрузки RSS:", err)
        return
    }

    for _, item := range rssObject.Items {
        if !newsExists(item.Link) {
            insertNews(item)
            broadcastUpdate(item) // Отправка обновления на WebSocket
        }
    }
    broadcastTableState() // Рассылка текущего состояния после обновления
}

// Обработчик для WebSocket подключения
func wsHandler(w http.ResponseWriter, r *http.Request) {
    conn, err := wsUpgrader.Upgrade(w, r, nil)
    if err != nil {
        log.Println("Ошибка WebSocket подключения:", err)
        return
    }
    defer conn.Close()

    // Добавляем соединение в хранилище
    connectionsMutex.Lock()
    connections[conn] = true
    connectionsMutex.Unlock()

    // Удаляем соединение при закрытии
    for {
        _, _, err := conn.ReadMessage()
        if err != nil {
            connectionsMutex.Lock()
            delete(connections, conn)
            connectionsMutex.Unlock()
            log.Println("WebSocket соединение закрыто:", err)
            break
        }
    }
}

// Отправка обновления на WebSocket всем подключенным клиентам
func broadcastUpdate(item *rss.Item) {
    // Подготавливаем данные в формате JSON
    newsData := map[string]string{
        "title":       item.Title,
        "link":        item.Link,
        "description": item.Summary,
        "pubDate":     item.Date.Format("2006-01-02 15:04:05"),
    }
    newsJSON, err := json.Marshal(newsData)
    if err != nil {
        log.Println("Ошибка при сериализации данных новости:", err)
        return
    }

    // Отправляем обновления всем подключенным клиентам
    connectionsMutex.Lock()
    defer connectionsMutex.Unlock()
    for conn := range connections {
        err := conn.WriteMessage(websocket.TextMessage, newsJSON)
        if err != nil {
            log.Println("Ошибка при отправке сообщения через WebSocket:", err)
            conn.Close()
            delete(connections, conn) // Удаляем соединение при ошибке
        }
    }
}

func fetchNews() ([]map[string]string, error) {
    rows, err := db.Query("SELECT title, link, description, pubDate FROM iu9ivenkova")
    if err != nil {
        return nil, err
    }
    defer rows.Close()

    var newsList []map[string]string
    for rows.Next() {
        var title, link, description, pubDate string
        if err := rows.Scan(&title, &link, &description, &pubDate); err != nil {
            return nil, err
        }
        newsItem := map[string]string{
            "title":       title,
            "link":        link,
            "description": description,
            "pubDate":     pubDate,
        }
        newsList = append(newsList, newsItem)
    }
    return newsList, nil
}

func broadcastTableState() {
    newsList, err := fetchNews()
    if err != nil {
        log.Println("Ошибка при получении новостей:", err)
        return
    }

    newsJSON, err := json.Marshal(newsList)
    if err != nil {
        log.Println("Ошибка при сериализации новостей:", err)
        return
    }
	//log.Println("Отправка данных клиентам:", string(newsJSON)) // Лог отправляемых данных

    connectionsMutex.Lock()
    defer connectionsMutex.Unlock()
    for conn := range connections {
        if err := conn.WriteMessage(websocket.TextMessage, newsJSON); err != nil {
            log.Println("Ошибка при отправке данных через WebSocket:", err)
            conn.Close()
            delete(connections, conn)
        }
    }
}

// Запуск HTTP сервера
func main() {
    initDB()
    defer db.Close()

    http.HandleFunc("/ws", wsHandler)
    http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
        http.ServeFile(w, r, "index.html") // Обслуживаем HTML файл клиенту
    })

    go func() {
        for {
            updateNews()               // Проверка на новые новости и обновление БД
			time.Sleep(2 * time.Minute) // Интервал обновления
        }
    }()

	go func() {
		for { 
			broadcastTableState()       // Отправляем текущее состояние таблицы всем клиентам
            time.Sleep(30 * time.Second) // Интервал обновления
        }
    }()

    fmt.Println("Сервер запущен на порту 9000...")
    log.Fatal(http.ListenAndServe(":9000", nil))
}
