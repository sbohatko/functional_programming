import Data.Char 
import Data.List
-- імпорт стандартних модулів

countLetters :: String -> Char -> Int --оголошення функції із типами параметрів, останній тип є типом виводу
countLetters str c = length $ filter (== c) str --дія функції із параметрами str c

rep :: String -> String -> String --оголошення функції
rep "a" x = replace x "a" "1" --якщо параметром передано а та змінну х(може бути будь якою строкою) то виконується ця дія
rep "b" x = replace x "b" "2" -- якщо параметром передано б, то виконується цей код, так само з іншими
rep "c" x = replace x "c" "3"
rep "d" x = replace x "d" "4"
rep "e" x = replace x "e" "5"
rep "f" x = replace x "f" "6"
rep "g" x = replace x "g" "7"
rep "h" x = replace x "h" "8"
rep "i" x = replace x "i" "9"
rep "j" x = replace x "j" "10"
rep "k" x = replace x "k" "11"
rep "l" x = replace x "l" "12"
rep "m" x = replace x "m" "13"
rep "n" x = replace x "n" "14"
rep "o" x = replace x "o" "15"
rep "p" x = replace x "p" "16"
rep "q" x = replace x "q" "17"
rep "r" x = replace x "r" "18"
rep "s" x = replace x "s" "19"
rep "t" x = replace x "t" "20"
rep "u" x = replace x "u" "21"
rep "v" x = replace x "v" "22"
rep "w" x = replace x "w" "23"
rep "x" x = replace x "x" "24"
rep "y" x = replace x "y" "25"
rep "z" x = replace x "z" "26"

replace :: Eq a => [a] -> [a] -> [a] -> [a] --оголошення функції з параметрами що є деякими значеннями
replace [] _ _ = [] --якщо не передано параметри
replace s find repl = -- якщо передано деякі параметри s строка для пошуку, find - символи які треба замінити repl на що замінити
    if take (length find) s == find --якщо створений список довжини find зі строки пошуку дорівнює символам які треба знайти
        then repl ++ (replace (drop (length find) s) find repl) --якщо це так, то заміняємо підстроку на repl, ++ це конкатенація строк
        else [head s] ++ (replace (tail s) find repl) -- якщо ні, то передаємо строку без останнього елементу для подальшого пошуку, head взяття першого елементу списку, вданому випадку строки

main = do --блок main в якому виконується код

  let file = "file.txt" --ім'я файлу
  writeFile file "Prepared is me marianne pleasure likewise debating. Wonder an unable except better stairs do ye admire. His and eat secure sex called esteem praise. So moreover as speedily differed branched ignorant. Tall are her knew poor now does then. Procured to contempt oh he raptures amounted occasion. One boy assure income spirit lovers set."  -- записуємо текст у файл
  
  text <- readFile file --зчитуємо текст з файлу
  putStrLn  "Введіть літеру для заміни в тексті - "
  var <- getLine --беремо введене користувачем значення, вводити треба тільки англійську літеру
  putStrLn (rep var text) --вивод тексту с заміненим значенням

  putStrLn  "Кількість букв a у тексті - " --пошук частоти літери у тексті
  print (countLetters text 'a')
  putStrLn  "Кількість букв b у тексті - "
  print (countLetters text 'b')
  putStrLn  "Кількість букв c у тексті - "
  print (countLetters text 'c')
  putStrLn  "Кількість букв d у тексті - "
  print (countLetters text 'd')
  putStrLn  "Кількість букв e у тексті - "
  print (countLetters text 'e')
  putStrLn  "Кількість букв f у тексті - "
  print (countLetters text 'f')
  putStrLn  "Кількість букв g у тексті - "
  print (countLetters text 'g')
  putStrLn  "Кількість букв h у тексті - "
  print (countLetters text 'h')
  putStrLn  "Кількість букв i у тексті - "
  print (countLetters text 'i')
  putStrLn  "Кількість букв j у тексті - "
  print (countLetters text 'j')
  putStrLn  "Кількість букв k у тексті - "
  print (countLetters text 'k')
  putStrLn  "Кількість букв l у тексті - "
  print (countLetters text 'l')
  putStrLn  "Кількість букв m у тексті - "
  print (countLetters text 'm')
  putStrLn  "Кількість букв n у тексті - "
  print (countLetters text 'n')
  putStrLn  "Кількість букв o у тексті - "
  print (countLetters text 'o')
  putStrLn  "Кількість букв p у тексті - "
  print (countLetters text 'p')
  putStrLn  "Кількість букв q у тексті - "
  print (countLetters text 'q')
  putStrLn  "Кількість букв r у тексті - "
  print (countLetters text 'r')
  putStrLn  "Кількість букв s у тексті - "
  print (countLetters text 's')
  putStrLn  "Кількість букв t у тексті - "
  print (countLetters text 't')
  putStrLn  "Кількість букв u у тексті - "
  print (countLetters text 'u')
  putStrLn  "Кількість букв v у тексті - "
  print (countLetters text 'v')
  putStrLn  "Кількість букв w у тексті - "
  print (countLetters text 'w')
  putStrLn  "Кількість букв x у тексті - "
  print (countLetters text 'x')
  putStrLn  "Кількість букв y у тексті - "
  print (countLetters text 'y')
  putStrLn  "Кількість букв z у тексті - "
  print (countLetters text 'z')
  
  writeFile "newfile.txt" (rep var text) -- запис заміненого тексту в новий файл