package main

import (
        "bufio"
        "fmt"
        "golang.org/x/crypto/ssh"
        "log"
        "os"
        //"strings"
)

// Функция для подключения к SSH-серверу
func connectToSSH(user, password, host string, port int) (*ssh.Client, error) {
        config := &ssh.ClientConfig{
                User: user,
                Auth: []ssh.AuthMethod{
                        ssh.Password(password),
                },
                HostKeyCallback: ssh.InsecureIgnoreHostKey(),
        }

        address := fmt.Sprintf("%s:%d", host, port)
        client, err := ssh.Dial("tcp", address, config)
        if err != nil {
                return nil, fmt.Errorf("не удалось подключиться к SSH-серверу: %w", err)
        }
        return client, nil
}

// Функция для выполнения команды на сервере
func runCommand(client *ssh.Client, command string) (string, error) {
        session, err := client.NewSession()
        if err != nil {
                return "", fmt.Errorf("не удалось создать сессию: %w", err)
        }
        defer session.Close()

        output, err := session.CombinedOutput(command)
        if err != nil {
                return "", fmt.Errorf("ошибка при выполнении команды: %w", err)
        }

        return string(output), nil
}

// Интерфейс для чтения команд от пользователя
func interactiveShell(client *ssh.Client) {
        scanner := bufio.NewScanner(os.Stdin)
        fmt.Println("Подключено к SSH-серверу. Введите команды (например, makedir, removedir, list, move, remove, ping). Введите 'exit' для выхода.")

        for {
                fmt.Print("Введите команду: ")
                scanner.Scan()
                command := scanner.Text()
                if command == "exit" {
                        fmt.Println("Отключение от сервера...")
                        break
                }

                output, err := runCommand(client, command)
                if err != nil {
                        log.Printf("Ошибка выполнения команды: %v\n", err)
                } else {
                        fmt.Println("Результат:\n", output)
                }
        }
}

func main() {
        // Настройки подключения
        user := "OLYA"           // замените на имя пользователя
        password := "password"       // замените на пароль
        host := "185.102.139.168"          // адрес SSH-сервера
        port := 5410                 // порт SSH-сервера

        // Подключение к SSH-серверу
        client, err := connectToSSH(user, password, host, port)
        if err != nil {
                log.Fatalf("Не удалось подключиться к серверу: %v", err)
        }
        defer client.Close()

        // Запуск интерактивного режима
        interactiveShell(client)
}