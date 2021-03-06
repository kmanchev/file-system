# file-system
Scheme implementation of Unix-based commands. The model is Tree-like structure with implemented with lists.
Bulgarian Documentation 

# 1.Регламент:
Искаме реализация на основните команди  от терминала на уникс базираните системи : 
**- cd -** - променя текущата директория по подаден абсолютен или релативен път
**- rm -** - изтрива подаден файл,директория
**- mkdir -** - създава нова директория по пълен път до позицията и или само по име (създавайки я в текущата диреткория)
**- touch -** - създава нов файл. Ако само един аргумент - то той е името и файла е празен. Ако 2 аргумента - 2рия аргумент е съдържанието на файла.
**- cat -** - конкатенира съдържанията на един или повече файлове.

# 2.Реализация
Проектът е имплементиран на Scheme.
В основата му стоят  2 структури от данни :
**- current-dir -** - текуща директория върху която оперираме. Типа на данните е string.
**- file-system -** - лист от елементи . Всеки елемент е реализиран с 3 под-листа с по 2 елемента всеки : 
name - името на елемента - което е пълният път до root директорията 
**- type -** - тип на елемента (file or folder)
**- content -** - лист от елементи (ако типа е file - съдържанието му  е стринг, ако е folder - съдържанието му е други елементи със същата структура, явяващи се деца на конкретния елемент). По този начин реализираме дървовидна структура която модифицираме при командите mkdir, touch, rm . Модифицирането става с метода modify-file-system. 
Той от своя страна използва вградения метод set!


 ##### Реализация на mkdir

Функцията **- mkdir -** приема един аргумент за който има 2 възможности - или е пълен път до новата папка която искаме да създадем или само име на папка (в този случай директорията ще бъде създадена в текущата директория) .

**- mkdir-in-path -** - създаване на директория по оказан пълен път . Функцията проверява първо дали родителската директория съществува. Ако това е така, можем да продължим с логиката за създаване на директория. Това става като презапишем file-system с по новата му версия която вече съдържа директорията.
Презаписването се извършва с функцията build-new-file-system - тя приема един аргумент , който е data node който ще добавим в новата версия на дървото. Този data node е с обновен списък content който съдържа новата папка. Това обновяване се извършва от функцията add-to-**- content-in-entry -** - тя приема 2 аргумента - новият content който се добавя при вече съществуващите и след това се конструира новия node с вече обновен content, Другият аргумент на функцията е node-а към който искаме да добавим ново съдържание.
**- mkdir-in-current-dir -** - приема 1 аргумент който е име на папката. Логиката тук е подобна като при създаване на директория по оказан пълен път. Разликата е че подаваме като аргумент за пътя на новата директория - current-dir + "/" + dir (името на папката)
**- pwd -** - просто отпечатваме стойноста на current-dir

**- ls -** - приема един или нито един аргумент. Когато не е подаден аргумент, отпечатваме всички файлове и директории от текущата папка.
Ако е подаден аргумент - то той трябва да бъде точен път до папката на която искаме да отпечатаме съдържанието .
За реализацията на достъпване на конкретен елемент от дървото, използваме няколко функции които също се ползват на други места в програмата. Те са: 
**- get-entry-list -** - връща лист на content-a за даденo entry (node) . 
**- get-node-by-full-path -** - достъпваме кой да е елемент от дървото по подаден пълен път до него.
