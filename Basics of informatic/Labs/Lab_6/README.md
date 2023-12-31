# Лабораторная работа № 6 «Основы синтаксического и лексического анализа» #
Дубанов А.В., Коновалов А.В.

18 ноября 2019

# Цель работы #
Получение навыков реализации лексических анализаторов и нисходящих синтаксических анализаторов, использующих метод рекурсивного спуска.

# Задания #
1. Реализуйте простейшие сканеры:

* Процедуру ```check-frac```, принимающую на вход строку и возвращающую ```#t```, если в строке записана простая дробь в формате десятичное-целое-со-знаком/десятичное-целое-без-знака, и ```#f``` в противном случае.

* Процедуру ```scan-frac```, принимающую на вход строку и возвращающую значение, если в строке записана простая дробь в формате десятичное-целое-со-знаком/десятичное-целое-без-знака, и ```#f``` в противном случае.

* Процедуру ```scan-many-fracs```, принимающую на вход строку, содержащую простые дроби, разделенные пробельными символами (строка также может начинаться и заканчиваться произвольным числом пробелов, символов табуляции, перевода строки и др.), и возвращающую список этих дробей. Если разбор невозможен, процедура должна возвращать ```#f```.

Примеры вызова процедур:
```scheme
(check-frac "110/111") ⇒ #t
(check-frac "-4/3")    ⇒ #t
(check-frac "+5/10")   ⇒ #t
(check-frac "5.0/10")  ⇒ #f
(check-frac "FF/10")   ⇒ #f

(scan-frac "110/111")  ⇒ 110/111
(scan-frac "-4/3")     ⇒ -4/3
(scan-frac "+5/10")    ⇒ 1/2
(scan-frac "5.0/10")   ⇒ #f
(scan-frac "FF/10")    ⇒ #f

(scan-many-fracs
 "\t1/2 1/3\n\n10/8")  ⇒ (1/2 1/3 5/4)
(scan-many-fracs
 "\t1/2 1/3\n\n2/-5")  ⇒ #f
```
При решении этих задач запрещается пользоваться процедурой ```string->number```, однако, пользоваться процедурами ```char->integer``` и ```integer->char``` можно и нужно.

Правильно реализованные процедуры должны быть однопроходными, т.е. выполнять один проход по входным данным.

Процедурам, обрабатывающим нетерминалы должны предшествовать комментарии с соответствующими правилами БНФ.

Рекомендация. Символ, маркирующий конец последовательности, выберете исходя из того, что на вход вашего лексера может поступить любая последовательность символов из таблицы ASCII, встречающаяся в текстовых файлах.

2. Реализуйте процедуру parse, осуществляющую разбор программы на модельном языке, представленной в виде последовательности (вектора) токенов (см. Лабораторную работу №4 «Интерпретатор стекового языка программирования»). Процедура parse должна включать в себя реализацию синтаксического анализа последовательности токенов методом рекурсивного спуска согласно следующей грамматикe:
```
<Program>  ::= <Articles> <Body> .
<Articles> ::= <Article> <Articles> | .
<Article>  ::= define word <Body> end .
<Body>     ::= if <Body> endif <Body> | integer <Body> | word <Body> | .
```
Процедура должна возвращать синтаксическое дерево в виде вложенных списков, соответствующих нетерминалам грамматики. В случае несоответствия входной последовательности грамматике процедура должна возвращать ```#f```. Примеры применения процедуры:

```scheme
(parse #(1 2 +)) ⇒ (() (1 2 +))

(parse #(x dup 0 swap if drop -1 endif))
    ⇒ (() (x dup 0 swap (if (drop -1))))

(parse #( define -- 1 - end
          define =0? dup 0 = end
          define =1? dup 1 = end
          define factorial
              =0? if drop 1 exit endif
              =1? if drop 1 exit endif
              dup --
              factorial
              *
          end
          0 factorial
          1 factorial
          2 factorial
          3 factorial
          4 factorial ))
 ⇒
 (((-- (1 -))
   (=0? (dup 0 =))
   (=1? (dup 1 =))
   (factorial
    (=0? (if (drop 1 exit)) =1? (if (drop 1 exit)) dup -- factorial *)))
  (0 factorial 1 factorial 2 factorial 3 factorial 4 factorial))
(parse #(define word w1 w2 w3)) ⇒ #f
```

Подготовьте еще 2-3 примера для демонстрации. Обратите внимание, что грамматика позволяет записывать на исходном языке вложенные конструкции ```if … endif```. Учтите эту особенность при реализации парсера и продемонстрируйте её на примерах.

**Ачивка (+1 балл)**: Модифицировать грамматику таким образом, чтобы она допускала вложенные статьи.

**Ачивка (+1 балл)**: Реализовать процедуру семантической проверки — программы с неопределёнными словами должны отвергаться. Допустимыми словами являются встроенные слова из предыдущей лабораторной работы + слова, определённые пользователем при помощи define.

**Штрафное задание.** Тем, кто будет сдавать лабораторную после Нового Года нужно будет добавить поддержку ветки else в if по правилу
```
<Body> ::= if <Body> <ElsePart> endif <Body>
       | integer <Body> | word <Body> | .
<ElsePart> ::= else <Body> | .
 ```
В синтаксическом дереве на выходе if без альтернативной ветви остаётся прежним, с альтернативной ветвью добавляется второй список.

```scheme
(parse #(if a b c d endif)) ⇒ (if (a b c d))
(parse #(if a b else c d endif)) ⇒ (if (a b) (c d))
```
А до Нового года — ачивка (+1 балл).
