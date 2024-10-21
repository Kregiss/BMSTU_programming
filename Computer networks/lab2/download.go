package main

import (
        "golang.org/x/net/html"
        "net/http"
        "strings"
        "fmt"
)

const baseURL = "https://dota2.ru/forum/forums/zhelezo-novosti-i-obsuzhdenija.166/"

type Topic struct {
        Title string
        Link  string
}

func FetchTopics() ([]Topic, error) {
        resp, err := http.Get(baseURL)
        if err != nil {
                return nil, fmt.Errorf("error fetching the forum page: %v", err)
        }
        defer resp.Body.Close()

        doc, err := html.Parse(resp.Body)
        if err != nil {
                return nil, fmt.Errorf("error parsing HTML: %v", err)
        }

        var topics []Topic
        var f func(*html.Node)
        f = func(n *html.Node) {
                if n.Type == html.ElementNode && n.Data == "a" {

                        for _, attr := range n.Attr {
                                if attr.Key == "href" && strings.Contains(attr.Val, "/threads/") {
                                        title := extractText(n)
                                        link := attr.Val

                                        if !strings.HasPrefix(link, "http") {
                                                link = "https://dota2.ru" + link
                                        }
                                        topics = append(topics, Topic{Title: title, Link: link})
                                }
                        }
                }

                for c := n.FirstChild; c != nil; c = c.NextSibling {
                        f(c)
                }
        }
        f(doc)

        return topics, nil
}

func extractText(n *html.Node) string {
        if n.Type == html.TextNode {
                return n.Data
        }
        var text string
        for c := n.FirstChild; c != nil; c = c.NextSibling {
                text += extractText(c)
        }
        return text
}