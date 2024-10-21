package main

import (
        "html/template"
        "log"
        "net/http"
)

var tmpl = template.Must(template.New("topics").Parse(`
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Forum Topics</title>
</head>
<body>
    <h1>Forum Topics</h1>
    <ul>
        {{range .}}
            <li><a href="{{.Link}}" target="_blank">{{.Title}}</a></li>
        {{else}}
            <li>No topics found</li>
        {{end}}
    </ul>
</body>
</html>
`))

func handler(w http.ResponseWriter, r *http.Request) {
        topics, err := FetchTopics()
        if err != nil {
                http.Error(w, "Error fetching topics", http.StatusInternalServerError)
                return
        }

        err = tmpl.Execute(w, topics)
        if err != nil {
                log.Printf("template execution error: %v", err)
        }
}

func main() {
        http.HandleFunc("/", handler)
        log.Println("Server is running at http://185.102.139.161:5420")
        err := http.ListenAndServe("185.102.139.161:5420", nil)
        if err != nil {
                log.Fatal("ListenAndServe: ", err)
        }
}