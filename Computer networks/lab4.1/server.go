package main

import (
        "fmt"
        "io"
        "io/ioutil"
        "log"
        "os"
        "os/exec"
        "github.com/gliderlabs/ssh"
)

// Функция для выполнения команд в системе
func runCommand(cmd string, args ...string) string {
        out, err := exec.Command(cmd, args...).CombinedOutput()
        if err != nil {
                return fmt.Sprintf("Ошибка: %s\n%s", err, string(out))
        }
        return string(out)
}

func main() {
        ssh.Handle(func(s ssh.Session) {
        user := s.User()
        fmt.Printf("Пользователь %s подключился к серверу\n", user) // Логируем подключение

                commands := s.Command()
                if len(commands) < 1 {
                        io.WriteString(s, "Не указана команда\n")
                        return
                }

        fmt.Printf("Пользователь %s выполняет команду: %s\n", user, commands) // Логируем команду

                switch commands[0] {
                case "makedir":
                        if len(commands) < 2 {
                                io.WriteString(s, "Укажите директорию для создания\n")
                                return
                        }
                        err := os.Mkdir(commands[1], 0755)
                        if err != nil {
                                io.WriteString(s, fmt.Sprintf("Ошибка создания директории: %s\n", err))
                        } else {
                                io.WriteString(s, "Директория создана\n")
                        }

                case "removedir":
                        if len(commands) < 2 {
                                io.WriteString(s, "Укажите директорию для удаления\n")
                                return
                        }
                        err := os.Remove(commands[1])
                        if err != nil {
                                io.WriteString(s, fmt.Sprintf("Ошибка удаления директории: %s\n", err))
                        } else {
                                io.WriteString(s, "Директория удалена\n")
                        }

                case "list":
                        if len(commands) < 2 {
                                commands = append(commands, ".")
                        }
                        files, err := ioutil.ReadDir(commands[1])
                        if err != nil {
                                io.WriteString(s, fmt.Sprintf("Ошибка чтения директории: %s\n", err))
                                return
                        }
                        for _, file := range files {
                                io.WriteString(s, file.Name()+"\n")
                        }

                case "move":
                        if len(commands) < 3 {
                                io.WriteString(s, "Укажите источник и место назначения\n")
                                return
                        }
                        err := os.Rename(commands[1], commands[2])
                        if err != nil {
                                io.WriteString(s, fmt.Sprintf("Ошибка перемещения файла: %s\n", err))
                        } else {
                                io.WriteString(s, "Файл перемещен\n")
                        }

                case "remove":
                        if len(commands) < 2 {
                                io.WriteString(s, "Укажите файл для удаления\n")
                                return
                        }
                        err := os.Remove(commands[1])
                        if err != nil {
                                io.WriteString(s, fmt.Sprintf("Ошибка удаления файла: %s\n", err))
                        } else {
                                io.WriteString(s, "Файл удален\n")
                        }

                case "ping":
                        if len(commands) < 2 {
                                io.WriteString(s, "Usage: ping <hostname>\n")
                                return
                        }
                        out := runCommand("ping", commands[1], "-c", "4")
                        io.WriteString(s, string(out))
                        log.Printf(user, "ping", commands[1], "success")
                default:
                        io.WriteString(s, "Неизвестная команда\n")
                }
        })

        log.Println("Запуск SSH-сервера на порту 5410...")
        err := ssh.ListenAndServe(":5410", nil, ssh.PasswordAuth(func(ctx ssh.Context, pass string) bool {
                return pass == "password"
        }))
        if err != nil {
                log.Fatal("Ошибка запуска SSH-сервера:", err)
        }
}