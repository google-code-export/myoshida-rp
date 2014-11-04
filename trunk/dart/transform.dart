

int main() {

  //// 逐次処理
  print("\n== each == ");
  
  var lis = ["foo", "bar", "baz"];
  for (var cnt = 0 ; cnt < lis.length ; cnt++) {
    print(lis[cnt]);
  }
  // foo
  // bar
  // baz
  
  print('');
  for (var elem in lis) {
    print(elem);
  }

  print('');
  lis.forEach( (elem) => print(elem) );
  // 以下も同じ
  lis.forEach( (elem) { print(elem); } );

  
  //// 写像(map)
  print("\n== map == ");

  var src = [3, 2, 9, 6];
  var mapped = src.map((elem) => elem * 2); // [6, 4, 18, 12]
  print(mapped.join(', '));


  //// フィルター(where)
  print("\n== where == ");

  src = [3, 2, 9, 6];
  var filtered = src.where((elem) => elem % 2 == 1); // [3, 9]
  print(filtered.join(', '));


  //// ソート
  print("\n== sort == ");

  lis = [-3, 2, -9, 0];
  lis.sort();                                     // [0, 2, -3, -9]
  print(lis.join(', '));
  lis.sort((x, y) => x.abs().compareTo(y.abs())); // [0, 2, -3, -9]
  print(lis.join(', '));


  //// 畳み込み(fold, reduce)
  print("\n== fold, reduce == ");
  src = [3, 2, 9, 6];
  var sumval = src.reduce((sum, elem) => sum + elem);                // 20
  print(sumval);
  var maxval = src.reduce((max, elem) => (max < elem) ? elem : max); // 9
  print(maxval);

  src = [3, 2, 9, 6];
  var leng = src.fold(0, (count, elem) => count+1);  // 4
  print(leng);
  var cons = src.fold("", (str, elem) => str += elem.toString() + ' '); // "3 2 9 6 "
  print(cons);

  print("");


  //// その他
  print("\n== etc == ");

  print("\n length, isEmpty");
  print(src.length);  // 4
  print(src.isEmpty); // false
  
  print("\n take");
  print(src.take(2)); // (3, 2)

  print("\n firstWhere, lastWhere");
  print(src.firstWhere((elem) => elem % 2 == 1)); // 3
  print(src.lastWhere( (elem) => elem % 2 == 1)); // 9

  print("\n contains");
  print(src.contains(9));  // true
  print(src.contains(5));  // false

  print("\n any, every");
  print(src.any(  (elem) => elem % 3 == 0)); // true
  print(src.every((elem) => elem % 3 == 0)); // false


  //// 連結
  print("\n== connect == ");
  var result = src.map((elem) => elem * 2);
  print(result);              // (6, 4, 18, 12)
  print(result.runtimeType);  // MappedListIterable

  src.where((elem) => elem % 2 == 1)
    .map((elem) => elem * 2)
    .forEach((elem) => print(elem));
  // 6
  // 18

  var resultsort = src.where((elem) => elem % 2 == 1)
    .map((elem) => elem * 2)
    .toList();
  resultsort.sort((x, y) => y.compareTo(x));
  print(resultsort); // [18, 6]
  

  
}
