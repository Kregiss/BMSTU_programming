package main

import (
  "fmt"
  "golang.org/x/net/html"
  "io"
  "io/ioutil"
  "log"
  "net/http"
  "os"
  "path/filepath"
  "strings"
)

var (
  LOCAL_HREF = "http://localhost:9000/?url="
  LOCAL_SRC  = "src=\"http://localhost:9000/static/"
)

func Handler(w http.ResponseWriter, r *http.Request) {
  url := r.FormValue("url")
  if url == "" {
    http.ServeFile(w, r, "index.html")
  } else {

    client := &http.Client{}
    req, err := http.NewRequest("GET", url, nil)
    if err != nil {
      log.Fatal(err)
    }

    resp, err := client.Do(req)
    if err != nil {
      log.Fatal(err)
    }
    defer resp.Body.Close()

    if resp.StatusCode != http.StatusOK {
      http.Error(w, "Failed to load the page", http.StatusBadGateway)
      return
    }

    body, err := ioutil.ReadAll(resp.Body)
    if err != nil {
      log.Fatal(err)
    }

    contentType := resp.Header.Get("Content-Type")
    if strings.HasPrefix(contentType, "text/html") {
      // Парсинг HTML, загрузка ресурсов
      modifiedHTML := ParseAndDownloadResources(url, string(body))
      w.Header().Set("Content-Type", contentType)
      fmt.Fprintf(w, modifiedHTML)  // Возвращаем модифицированный HTML
    } else {
      // Просто передаем не HTML контент
      w.Header().Set("Content-Type", contentType)
      w.Write(body)
    }
  }
}

// Функция для парсинга HTML и скачивания ресурсов
func ParseAndDownloadResources(baseURL, body string) string {
  tokenizer := html.NewTokenizer(strings.NewReader(body))
  var modifiedHTML strings.Builder

  for {
    tt := tokenizer.Next()
    switch tt {
    case html.ErrorToken:
      if tokenizer.Err() == io.EOF {
        return modifiedHTML.String()  // Возвращаем собранный HTML
      }
      log.Fatal(tokenizer.Err())
    case html.StartTagToken, html.SelfClosingTagToken:
      t := tokenizer.Token()
      modifiedHTML.WriteString("<" + t.Data)  // Начало тега

      for _, attr := range t.Attr {
        attrStr := fmt.Sprintf(` %s="%s"`, attr.Key, attr.Val)
        if (t.Data == "img" && attr.Key == "src") ||
          (t.Data == "link" && attr.Key == "href" && strings.HasSuffix(attr.Val, ".css")) ||
          (t.Data == "script" && attr.Key == "src") {

          // Обрабатываем src и href для картинок, стилей и скриптов
          resourceURL := ResolveURL(baseURL, attr.Val)
          filePath := DownloadAndCacheResource(resourceURL)

          if filePath != "" {
            attrStr = fmt.Sprintf(` %s="/static/%s"`, attr.Key, filePath)  // Обновляем ссылку на локальную
          }
        } else if t.Data == "a" && attr.Key == "href" {  // Обрабатываем ссылки <a>
          // Перенаправляем все ссылки через локальный сервер
          newHref := ResolveURL(baseURL, attr.Val)
          attrStr = fmt.Sprintf(` %s="%s%s"`, attr.Key, LOCAL_HREF, newHref)
        }
        modifiedHTML.WriteString(attrStr)  // Добавляем атрибуты тега
      }

      if t.Data[len(t.Data)-1] == '/' {
        modifiedHTML.WriteString("/>")  // Закрытие тега
      } else {
        modifiedHTML.WriteString(">")  // Открытие тега
      }
    case html.TextToken:
      modifiedHTML.Write(tokenizer.Text())  // Добавляем текст внутри тегов
    case html.EndTagToken:
      t := tokenizer.Token()
      modifiedHTML.WriteString("</" + t.Data + ">")  // Закрытие тега
    }
  }
  return modifiedHTML.String()  // Возвращаем модифицированный HTML
}

// Функция для скачивания ресурса и сохранения его локально
func DownloadAndCacheResource(url string) string {
  fileName := filepath.Base(url)
  filePath := filepath.Join("static", fileName)

  
  if _, err := os.Stat(filePath); os.IsNotExist(err) {
    
    resp, err := http.Get(url)
    if err != nil {
      log.Println("Failed to download resource:", err)
      return ""
    }
    defer resp.Body.Close()

    out, err := os.Create(filePath)
    if err != nil {
      log.Println("Failed to save resource:", err)
      return ""
    }
    defer out.Close()


    _, err = io.Copy(out, resp.Body)
    if err != nil {
      log.Println("Failed to copy resource to file:", err)
      return ""
    }
  }
  return fileName
}

// Функция для корректного создания полного URL
func ResolveURL(baseURL, relativeURL string) string {
  if strings.HasPrefix(relativeURL, "http") {
    return relativeURL
  }
  if strings.HasPrefix(relativeURL, "/") {
    // Если ссылка относительная и начинается с "/", добавляем протокол и домен
    base := baseURL[:strings.Index(baseURL, "//")+2] + strings.Split(baseURL, "/")[2]
    return base + relativeURL
  }
  return baseURL + relativeURL
}

func main() {
  // Создаем папку для хранения загруженных ресурсов, если она не существует
  os.Mkdir("static", 0755)

  http.HandleFunc("/", Handler)
  http.Handle("/static/", http.StripPrefix("/static/", http.FileServer(http.Dir("static"))))
  log.Fatal(http.ListenAndServe(":9000", nil))
}