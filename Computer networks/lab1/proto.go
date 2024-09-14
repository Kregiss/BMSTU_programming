package proto

import "encoding/json"

type Request struct {
        Command string `json:"command"`
        Data *json.RawMessage `json:"data"`

}

type Response struct {
        Status string `json:"status"`
        Data *json.RawMessage `json:"data"`
}

type Polynomial struct {
        Degree       int       `json:"degree"`       
        Coefficients []float64 `json:"coefficients"` 
        X            float64   `json:"x"`            
}
