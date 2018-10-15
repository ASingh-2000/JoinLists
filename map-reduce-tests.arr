# CSCI0190 (Fall 2018)
include shared-gdrive("map-reduce-support.arr", "1F87DfS_i8fp3XeAyclYgW3sJPIf-WRuH")
  
import map-reduce,
       anagram-map,
       anagram-reduce,
       recommend,
       popular-pairs
  from my-gdrive('map-reduce-code.arr')

# DO NOT CHANGE ANYTHING ABOVE THIS LINE
fun count<A>(target :: A, a :: List<A>, eq-checker :: (A, A -> Boolean))
  -> Number:
  doc: "counts quantity of target in a"
  fun el-checker(el, cnt):
    if eq-checker(el, target):
      cnt + 1
    else:
      cnt
    end
  end
  a.foldl(el-checker, 0)
where:
  count(3, [list: 1, 2, 3], lam(x, a): x == a end) is 1
  count(3, [list: 3, 2, 3], lam(x, a): x == a end) is 2
  count(4, [list: 1, 2, 3], lam(x, a): x == a end) is 0
end

fun lst-same-els<A>(
    a :: List<A>,
    b :: List<A>,
    eq-checker :: (A, A -> Boolean))
  -> Boolean:
  doc: "checks whether two lists have the same elements in the same quantity"
  fun same-count(el, acc):
    acc and (count(el, a, eq-checker) == count(el, b, eq-checker))
  end
  (a.length() == b.length()) and a.foldl(same-count, true)
where:
  lst-same-els([list: 1, 2, 3], [list: 1, 2, 3], lam(x, a): x == a end)
    is true
  lst-same-els([list: 1, 2, 3], [list: 1, 2, 3], lam(x, a): x == a end)
    is true
  lst-same-els([list: 1, 2, 3], [list: 2, 1, 3], lam(x, a): x == a end)
    is true
  lst-same-els([list: 1, 2, 2], [list: 2, 1], lam(x, a): x == a end)
    is false
  lst-same-els([list: 1, 2, 2], [list: 2, 1, 1], lam(x, a): x == a end)
    is false
end
 
fun recommend-equiv(
    t1 :: Tv-pair<Number, List<String>>,
    t2 :: Tv-pair<Number, List<String>>)
  -> Boolean:
  doc: "checks whether two recommenations are equivalent"
  (t1.tag == t2.tag) and lst-same-els(t1.value, t2.value, lam(x, y): x == y end)
where:
  recommend-equiv(tv(0, empty), tv(0, empty)) is true
  recommend-equiv(tv(1, [list: "a"]), tv(1, [list: "a"])) is true
  recommend-equiv(tv(1, [list: "a", "b"]), tv(1, [list: "a", "b"]))
    is true
  recommend-equiv(tv(1, [list: "b", "a"]), tv(1, [list: "a", "b"]))
    is true
  recommend-equiv(tv(1, [list: "b", "a"]), tv(2, [list: "a", "b"]))
    is false
end

fun popular-equiv(
    t1 :: Tv-pair<Number, List<String>>,
    t2 :: Tv-pair<Number, List<String>>)
  -> Boolean:
  doc: "checks whether two popular pairs are equivalent"
  fun equiv-parse(str :: String, delim :: String) -> List:
    if string-length(str) < string-length(delim):
      empty
    else if string-substring(str, 0, string-length(delim)) == delim:
      substr = string-substring(str, string-length(delim), string-length(str))
      parse(substr, delim)
    else:
      split = string-split-at(str, delim)
      link(split.word, parse(split.rest, delim))
    end
  end
  (t1.tag == t2.tag) and lst-same-els(t1.value, t2.value,
    lam(str1, str2):
      lst-same-els(equiv-parse(str1, "+"), equiv-parse(str2, "+"),
        lam(x, y): x == y end)
    end)
where:
  popular-equiv(tv(0, empty), tv(0, empty)) is true
  popular-equiv(tv(1, [list: "a+b"]), tv(1, [list: "a+b"])) is true
  popular-equiv(tv(1, [list: "a+b"]), tv(1, [list: "b+a"])) is true
  popular-equiv(tv(1, [list: "a+b", "b+c"]), tv(1, [list: "b+c", "a+b"]))
    is true
  popular-equiv(tv(1, [list: "a+b", "c+b"]), tv(1, [list: "b+c", "a+b"]))
    is true
  popular-equiv(tv(2, [list: "a+b", "c+b"]), tv(1, [list: "b+c", "a+b"]))
    is false
end


#------------------------------------------------------------------------------#
check "map-reduce tests":
  #The following is a simple test for map-reduce
  map-reduce([list: tv("book1", "the")], wc-map, wc-reduce)
    is [list: tv("the", 1)]
  #The following is a check for one tv in the list
  lst-same-els(map-reduce([list: tv("book1", "the book is the key")], 
      wc-map, wc-reduce), [list: tv("the", 2), tv("book", 1), tv("is", 1), 
      tv("key", 1)], lam(x, a): x == a end) is true
  #The following is a more complex test for map-reduce
  lst-same-els(map-reduce([list:
        tv("Book1", "Harry Potter and the Philosopher's Stone"),
        tv("Book2", "Harry Potter and the Chamber of Secrets"),
        tv("Book3", "Harry Potter and the Prisoner of Azkaban"),
        tv("Book4", "Harry Potter and the Goblet of Fire"),
        tv("Book5", "Harry Potter and the Order of the Phoenix"),
        tv("Book6", "Harry Potter and the Half-Blood Prince"),
        tv("Book7", "Harry Potter and the Deathly Hallows")], 
      wc-map, wc-reduce), 
    [list: tv("Harry", 7), tv("Potter", 7), tv("and", 7), tv("the", 8), 
      tv("of", 4), tv("Philosopher's", 1), tv("Stone", 1), tv("Chamber", 1), 
      tv("Secrets", 1), tv("Prisoner", 1), tv("Azkaban", 1), tv("Goblet", 1), 
      tv("Fire", 1), tv("Order", 1), tv("Phoenix", 1), tv("Half-Blood", 1), 
      tv("Prince", 1), tv("Deathly", 1), tv("Hallows", 1)], 
    lam(x, a): x == a end) is true
end

fun anagram-list(tv-pair :: List<Tv-pair>)
  -> List<List>:
  doc: "Converts a list of tv-pair to a list of their values"
  cases (List) tv-pair:
    | empty => empty
    | link(f, r) => link(f.value, anagram-list(r))
  end
end

fun is-equiv(new-tv :: List<String>, new-check-list :: List<List>) 
  -> Boolean:
  doc: "Checks if a tv's list is inside the list of tv-lists"
  cases (List) new-check-list:
    | empty => false
    | link(f, r) =>
      if lst-same-els(new-tv, f, lam(x, a): x == a end):
        true
      else:
        is-equiv(new-tv, r)
      end
  end
end

fun anagram-check(tv-list :: List<Tv-pair>, check-list :: List<Tv-pair>)
  -> Boolean:
  doc: "Checks if the tv-list is equivalent to the expected check-list"
  a-list=anagram-list(tv-list)
  b-list=anagram-list(check-list)
  bool-list = 
    a-list.map(lam(x): is-equiv(x, b-list) end)
  not(bool-list.member(false))
end

check "anagram tests":
  #Designed as a normal test; checks character-case and repetition
  anagram-check(map-reduce([list:
        tv("file1", "box cat dog Sass"), tv("file2", "obx act odg aSss"),
        tv("file3", "oxb atc ogd xob Ssas box"), 
        tv("file4", "tac god bxo sass")],
      anagram-map, anagram-reduce), 
    [list: 
      tv("box-group", [list: "box", "obx", "oxb", "xob", "bxo"]),
      tv("cat-group", [list: "cat", "act", "atc", "tac"]),
      tv("dog-group", [list: "dog", "odg", "ogd", "god"]),
      tv("Sass-group", [list: "Sass", "aSss", "Ssas"]),
      tv("sass-group", [list: "sass"])]) is true
end

check "recommend tests":
  #The following tests for a book not in any files
  recommend-equiv(recommend("book5", 
      [list: tv("file1", "book1\nbook2\nbook3\nbook4"), 
        tv("file2", "book1\nbook3\nbook4"), 
        tv("file3", "Book1\nbook3\nbook4")]),
    tv(0, [list: ])) is true
  #The following is a simple check for recommend
  recommend("Animal Farm", [list: tv("file1", "1984\nAnimal Farm")]) is 
  tv(1, [list: "1984"])
  #More complicated tests follow
  recommend-equiv(recommend("book1",
      [list: tv("file1", "book1\nbook2\nbook3\nbook4"),
        tv("file2", "book1\nbook3\nbook4"),
        tv("file3", "Book1\nbook3\nbook4")]),
    tv(2, [list: "book3", "book4"])) is true
end

check "popular tests":
  #The following checks no pairs
  #The following is a simple check for one pair
  popular-equiv(popular-pairs([list: tv("file1", "book1\nbook2\nbook3\nbook4"), 
      tv("file2", "book1\nbook3\nbook4"), 
      tv("file3", "Book1\nbook3\nbook4")]),
  tv(3, [list: "book3+book4"])) is true
  #The following checks for multiple solutions
  popular-equiv(popular-pairs([list: tv("file1", "book1\nbook2\nbook3\nbook4"), 
      tv("file2", "book1\nbook3\nbook5"), 
      tv("file3", "book1\nbook3\nbook4"),
      tv("file4", "book1\nbook4"),
      tv("file3", "book3\nbook4")]), 
  tv(3, [list: "book1+book3", "book1+book4", "book3+book4"])) is true
end

```popular-pairs([list: tv("file1", "book1"),
        tv("file2", "book2"),
      tv("file3", "book3")]) is tv(0, [list: ])```
