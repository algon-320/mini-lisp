# mini-lisp

簡易LISPインタプリタ

## 実行

- `$ cargo build --release`
- `$ ./target/release/mini-lisp ./examples/hello.mlisp # ファイルから`
- `$ ./target/release/mini-lisp -c '(println "Hello, World")' # コマンドライン引数から`


## 式の評価

- リストの先頭を評価する
- リストの先頭が関数なら関数を呼び出し、結果を返す
    - 引数は基本的に左から右の順に評価される
- リストの先頭が関数でないなら、リストをそのまま返す