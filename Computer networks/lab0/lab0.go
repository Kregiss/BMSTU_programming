package main

import (
    "fmt"
    "github.com/SlyMarbo/rss"
    "log"
    "net/http"
    "strings"
)

func HomeRouterHandler(w http.ResponseWriter, r *http.Request) {
    r.ParseForm()
    fmt.Println(r.Form)
    fmt.Println("path", r.URL.Path)
    fmt.Println("scheme", r.URL.Scheme)
    fmt.Println(r.Form["url_long"])

    for k, v := range r.Form {
        fmt.Println("key:", k)
        fmt.Println("val:", strings.Join(v, ""))
    }

    rssUrl := "https://www.bragazeta.ru/feed/"

    rssObject, _ := rss.Fetch(rssUrl)

    html := "<html><body>"
    html += "<h1>" + rssObject.Title + "</h1>"
    html += "<p>" + rssObject.Description + "</p>"
    html += "<ul>"

    for i, item := range rssObject.Items {
        html += "<li>"
        html += fmt.Sprintf("%d. <a href=%s>%s</a><br>", i+1, item.Link, item.Title)
        if item.Summary != "" {
            html += fmt.Sprintf("<p>%s</p>", item.Summary)
        }
        html += "</li>"
    }

    html += "</ul>"
    html += "</body></html>"

    fmt.Fprintf(w, html)
}

func main() {
    http.HandleFunc("/", HomeRouterHandler) 
    fmt.Println("Server is listening on port 9000...")
    err := http.ListenAndServe(":9000", nil) 
	if err != nil {
        log.Fatal("ListenAndServe: ", err)
    }
}
