package main

import (
  "bufio"
  "fmt"
  "io"
  "log"
  "os"
  "strings"
  "time"
  "github.com/jlaffaye/ftp"
)

func connect() (*ftp.ServerConn, error) {

  conn, err := ftp.Dial("students.yss.su:21", ftp.DialWithTimeout(5*time.Second))
  if err != nil {
    return nil, err
  }

  err = conn.Login("ftpiu8", "3Ru7yOTA")
  if err != nil {
    return nil, err
  }

  fmt.Println("Успешно подключен к FTP-серверу")
  return conn, nil
}

func uploadFile(conn *ftp.ServerConn, localPath, remotePath string) error {
  file, err := os.Open(localPath)
  if err != nil {
    return err
  }
  defer file.Close()

  err = conn.Stor(remotePath, file)
  if err != nil {
    return err
  }

  fmt.Println("Файл успешно загружен")
  return nil
}

func downloadFile(conn *ftp.ServerConn, remotePath, localPath string) error {
  resp, err := conn.Retr(remotePath)
  if err != nil {
    return err
  }
  defer resp.Close()

  file, err := os.Create(localPath)
  if err != nil {
    return err
  }
  defer file.Close()

  _, err = io.Copy(file, resp)
  if err != nil {
    return err
  }

  fmt.Println("Файл успешно скачан")
  return nil
}

func createDirectory(conn *ftp.ServerConn, dirName string) error {
  err := conn.MakeDir(dirName)
  if err != nil {
    return err
  }

  fmt.Println("Директория успешно создана")
  return nil
}

func deleteFile(conn *ftp.ServerConn, filePath string) error {
  err := conn.Delete(filePath)
  if err != nil {
    return err
  }

  fmt.Println("Файл успешно удален")
  return nil
}

func listDirectory(conn *ftp.ServerConn, dirPath string) error {
  entries, err := conn.List(dirPath)
  if err != nil {
    return err
  }

  for _, entry := range entries {
    fmt.Println(entry.Name)
  }

  return nil
}

func changeDirectory(conn *ftp.ServerConn, dirPath string) error {
  err := conn.ChangeDir(dirPath)
  if err != nil {
    return err
  }

  fmt.Println("Перешли в директорию:", dirPath)
  return nil
}

func removeEmptyDirectory(conn *ftp.ServerConn, dirPath string) error {
  err := conn.RemoveDir(dirPath)
  if err != nil {
    return err
  }

  fmt.Println("Пустая директория удалена")
  return nil
}

func removeDirectoryRecursively(conn *ftp.ServerConn, dirPath string) error {

  err := conn.RemoveDirRecur(dirPath)
  if err != nil {
    return fmt.Errorf("ошибка рекурсивного удаления директории %s: %v", dirPath, err)
  }
  return nil
}

func main() {
  conn, err := connect()
  if err != nil {
    log.Fatal("Не удалось подключиться к FTP-серверу:", err)
  }
  defer conn.Quit()

  reader := bufio.NewReader(os.Stdin)

  for {
    fmt.Print("> ")
    input, _ := reader.ReadString('\n')
    input = strings.TrimSpace(input)
    parts := strings.Split(input, " ")
    cmd := parts[0]

    switch cmd {
    case "mkdir":
      err := createDirectory(conn, parts[1])
      if err != nil {
        fmt.Println("Ошибка создания директории:", err)
      }

    case "cd":
      err := changeDirectory(conn, parts[1])
      if err != nil {
        fmt.Println("Ошибка перехода в директорию:", err)
      }

    case "ls":
      err := listDirectory(conn, ".")
      if err != nil {
        fmt.Println("Ошибка получения содержимого директории:", err)
      }

    case "upload":

      err := uploadFile(conn, parts[1], parts[2])
      if err != nil {
        fmt.Println("Ошибка загрузки файла:", err)
      } 

    case "download":

      err := downloadFile(conn, parts[1], parts[2])
      if err != nil {
        fmt.Println("Ошибка скачивания файла:", err)
      } 

    case "rm":

      err := deleteFile(conn, parts[1])
      if err != nil {
        fmt.Println("Ошибка удаления файла:", err)
      } 

    case "rmdir":

      err := removeEmptyDirectory(conn, parts[1])
      if err != nil {
        fmt.Println("Ошибка удаления директории:", err)
      } 

    case "rmdir_recursive":
      err := removeDirectoryRecursively(conn, parts[1])
      if err != nil {
        fmt.Println("Ошибка рекурсивного удаления директории:", err)
      } 

    case "exit":
      return

default:
      fmt.Println("Неизвестная команда:", cmd)
      fmt.Println("Доступные команды: mkdir, cd, ls, upload, download, rm, rmdir, rmdir_recursive, exit")
    }
  }
}