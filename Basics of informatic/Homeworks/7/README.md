# Домашнее задание № 7. «Оболочка и скрипты» #
Дубанов А.В., Коновалов А.В.

8 декабря 2019

Домашнее задание становится очно, но баллы по нему суммируются через сервер, поэтому нужно прислать отчёт. Отчёт оформляется также, как и лабораторная работа № 7.

# Домашнее задание №7 #
Скриптовый язык, на котором будет выполняться домашняя работа, студентом выбирается самостоятельно. Примеры возможных скриптовых языков: JavaScipt (Node.js), Python, Ruby, Lua, Perl, Racket и т.д.

Примем следующие критерии скриптового языка:

* в unix-среде файл можно сделать исполнимым и добавить в начало файла shebang (#!/путь/до/интерпретатора),
* в Windows расширение файла исходного текста можно добавить в PATHEXT и запускать из оболочки cmd.exe,
* Microsoft PowerShell (он из соображений безопасности в PATHEXT не добавляется, поэтому выписан отдельно).

**Язык лабораторной работы № 7, домашней работы № 7 и РК3 должен быть одним и тем же!**

# 1. Утилита ```tree``` #
Реализуйте собственный вариант утилиты tree. Пусть ваша программа поддерживает по меньшей мере ключи ```-d``` и ```-o``` так же, как реализация утилиты tree в ОС Linux. Поддержку других ключей можно не реализовывать.

Для «рисования» дерева в консоли используйте символы псевдографики.

Программа не должна аварийно завершаться, если права доступа запрещают получение списка файлов какого-либо каталога.

# 2. Утилита ```grep``` #
Реализуйте собственный вариант утилиты ```grep```. Допускается ограничиться работой только с текстовыми файлами. Так же, как и стандартная утилита grep, ваша программа должна обрабатывать как стандартный ввод, так и файлы, пути к которым указаны в командной строке. Ключ ```-e``` должен позволять передать программе регулярное выражение вместо строки для поиска. Пусть ваша реализация также поддерживает ключи ```-i```, ```-m```, ```-n``` так же, как это делает стандартная реализация утилиты grep. Поддержку других ключей можно не реализовывать.

Программа не должна аварийно завершаться, если какой-либо из файлов, перечисленных в аргументах командной строки, не может быть прочитан.

Сообщения об ошибках и предупреждения должны направляться в стандартный поток вывода ошибок. **Направление таких сообщений в стандартный поток вывода не допускается.**

# 3. Утилита ```wc``` #
Реализуйте собственный вариант утилиты ```wc```. Так же, как и стандартная утилита ```wc```, ваша программа должна обрабатывать как стандартный ввод, так и файлы, пути к которым указаны в командной строке. Пусть ваша реализация поддерживает ключи ```-c```, ```-m```, ```-w```, ```-l``` так же, как это делает стандартная реализация утилиты ```wc```. Поддержку других ключей можно не реализовывать.

Сообщения об ошибках и предупреждения должны направляться в стандартный поток вывода ошибок. **Направление таких сообщений в стандартный поток вывода не допускается.**

# 4. Поиск опечаток #
Реализуйте простейшую программу проверки орфографии. Пусть программа принимает на вход словарь и текст на естественном языке и выводит список и координаты слов (строка, колонка), которые не встречаются в словаре.

Например, пусть ```dictionary.txt``` — словарь, а ```example-missprint.txt``` — текст, где в строке 1 допущена опечатка в слове ```general```, во 2 строке — в слове ```emphasizes``` и в 7 строке — в слове ```supports``` (1-е буквы этих слов находятся в 25, 23 и 8 колонках соответственно). Тогда вызов и результат работы вашей программы ```speller.py``` должен выглядеть так:
```
> ./speller.py dictionary.txt example-missprint.txt
1,  25    gneral
2,  23    emphasises
7,   8    suports
```
Считайте, что в проверяемом тексте переносы слов отсутствуют. Различные формы одного слова рассматривайте как разные слова. Апостроф считайте частью слова.

# Рекомендации #
В виде отдельного модуля реализуйте сканер, преобразующий текст в токены — слова и знаки пунктуации. Для каждого токена храните его координаты в исходном тексте — позицию от начала текста, номер строки, номер колонки.

Тестирование программы выполните на примерах коротких английских текстов.

Словарь получите из текста, в котором, как вы считаете, отсутствуют опечатки. Для получения отдельных слов из этого текста используйте разработанный вами сканер. Напишите вспомогательную программу, которая будет строить словарь по тексту, поданному на вход этой программы.

# Ачивка #
В качестве скриптового языка выбрать какой-нибудь редкий или необычный язык:

* язык, отличный от Python — (1 балл),
* язык, отличный от Python и NodeJS — (2 балла)