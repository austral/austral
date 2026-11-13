# リファクタリングの前に測ったこと

`docs/refactoring-plan.md` の根拠である。コンパイラ（`lib/` と `bin/`）を、関数型のドメインモデリングの方法で作り直す前に、今の形を数え、振る舞いを固定する手段を試した。

スクリプトは `tools/refactoring/` にある。どれも再実行できる。数は `tools/refactoring/expected/` に記録してあり、スクリプトは今の木と比べて違いを報告する。作り直しの各項目は、`tools/refactoring/check.sh` で確かめる。

対象のコミットは `f408e59` である。`lib/` と `bin/` は、それ以降変わっていない。

この文書の数は、計画を作った時点のものである。カバレッジの数は、`lib/TypeParser.ml` と `lib/CliParser.ml` を誤って除いていた。名前が `Parser.ml` で終わるファイルを、生成された構文解析器と一緒に除いていたためである。2つを含めた計画時点の数は 77.27%（5273 / 6824）で、`tools/refactoring/baseline/coverage.txt` にある。その後、対象に `examples/` の 12 本を足したので、`tools/refactoring/expected/` の数は、ここより新しい。カバレッジは 77.33% から 77.88% になった。

## 何を確かめたか

| 問い | 答え | 確かめ方 |
|---|---|---|
| リファクタリングの前後で、コンパイラの出力が同じだと確かめられるか | 確かめられる。308 本のプログラムについて、生成した C、標準出力、標準エラー、終了コード、`error.html` の中身、残したファイルの名前を記録して比べる。12 秒で終わる | `equivalence.py` |
| 生成した C は、実行のたびに同じか | 同じである。同じコンパイラで2回記録して、308 本すべてが一致した | `equivalence.py` を2回 |
| その比較は、違いを見つけるか | 見つける。issue #16 と #22 を直す前のコンパイラと比べると、22 本が違った。直した規則を試すテストの 22 本と一致する | `equivalence.py compare` |
| 生成する名前の番号だけがずれた場合に、同じと判定できるか | できる。番号を現れた順に振り直してから比べる。番号を一律にずらした写しは、厳密な比較で 93 本が違い、振り直すと 0 本になった。2つの名前を取り違えた写しは、振り直しても 1 本が違った | `equivalence.py compare --normalize-ids` |
| 今のテストは、コンパイラのどれだけを実行しているか | 手で書いたコードの 77.33%。エラーを作る `TypeErrors.ml` は 20.69% | `coverage.sh` |
| 例外、可変の状態、入出力は、どこにどれだけあるか | 下の表 | `measure.sh` |
| 新しい構成子を黙って飲み込む match は、いくつあるか | 398 個、27 ファイル | `measure.sh --fragile` |
| 公開している関数のうち、外から使われていないものは、いくつあるか | 346 個のうち 68 個 | `unused-exports.sh` |

## 等価性の検査（`equivalence.py`）

```
python3 equivalence.py snapshot ./austral OUT_A
python3 equivalence.py snapshot path/to/other/austral OUT_B
python3 equivalence.py compare OUT_A OUT_B
```

- 対象は、end-to-end のテスト 307 本と、標準ライブラリ（テストを含む）1 本である。
- プログラムごとに、終了コード、生成した C、標準出力、標準エラー、`error.html` の中身、作業ディレクトリに残ったファイルの名前を保存する。パスは `$CWD` と `$ROOT` に置き換える。
- `calltree.html` は、名前だけを記録する。中身は、コンパイラの中の関数の呼び出しの記録で、構造を変えれば必ず変わる。
- コンパイラは、プログラムごとの一時ディレクトリで動かす。失敗したときに、作業ディレクトリへ `error.html` と `calltree.html` を書くためである。
- 308 本のうち、C が出るのは 93 本である。215 本は、コンパイルエラーを期待するテストである。エラーの文面も比べるので、エラーの出し方を変えると検出される。

**限界。** この検査が固定するのは、308 本が通る道だけである。テストが実行しないコードを動かしても、検査は何も言わない。次の節の数が、その大きさである。

### 生成する名前の番号

コンパイラは、生成する名前（`decl_286`、`mono_13`、`_t127`）の番号を、10 個の大域のカウンタから取る。同じ入力なら、同じ番号になる。関係のない関数を1つ足すと、後ろの番号がずれる。
採番のしかたを変える項目（計画の R16）では、番号だけが変わる。`compare --normalize-ids` は、名前の種類ごとに、番号を現れた順に振り直してから比べる。振り直しは1対1なので、番号のほかの違いは隠さない。

## カバレッジ（`coverage.sh`）

木の一時的な写しを bisect_ppx で計装してビルドし、`equivalence.py` と同じ 308 本を流す。リポジトリは変えない。

| 範囲 | 実行された点 | 割合 |
|---|---|---|
| 手で書いたファイル | 5077 / 6565 | 77.33% |
| 生成された構文解析器と組み込みモジュールを含む全体 | 7759 / 12046 | 64.41% |

実行されていない点が多いファイルは、次のとおりである。

| ファイル | 実行されていない点 | 割合 |
|---|---|---|
| `lib/TypeCheckExpr.ml` | 117 | 79.76% |
| `lib/TypingPass.ml` | 93 | 78.27% |
| `lib/Type.ml` | 78 | 57.84% |
| `lib/Monomorphize.ml` | 69 | 86.55% |
| `lib/TypeClasses.ml` | 69 | 56.60% |
| `lib/TypeErrors.ml` | 69 | 20.69% |
| `lib/LinearityCheck.ml` | 67 | 84.16% |
| `lib/CombiningPass.ml` | 61 | 72.15% |
| `lib/CodeGen.ml` | 53 | 88.82% |
| `lib/CliEngine.ml` | 53 | 47.00% |

`TypeErrors.ml` は、型エラーの文面を作る関数の集まりである。87 点のうち 18 点しか実行されない。型エラーの大半は、どのテストも起こしていない。

## 数えたもの（`measure.sh`）

生成される `lib/BuiltInModules.ml` を除く、76 ファイル、15280 行について数えた。

| 指標 | 数 | 意味 |
|---|---|---|
| `raise` | 476 | 例外を投げる箇所。`austral_raise`、`err`、`internal_err`、`Errors.x`、`failwith`、`raise`、`assert false` |
| `internal` | 93 | `raise` のうち、`internal_err` の数。「ここには来ないはず」とコードが考えている箇所である。型が、その状態を表せてしまっている |
| `adhoc` | 43 | `raise` のうち、`err "..."` の数。名前のないエラーである |
| `catch` | 56 | 例外を捕まえるか、位置を付け直す箇所。`try`、`adorn_error_with_*` |
| `state` | 15 | 可変の状態。`ref` のセルを作る箇所、`Hashtbl` を作る箇所、最上位の `Buffer`、`mutable` な欄の宣言 |
| `io` | 56 | 入出力とプロセスの制御。ファイル、表示、`Sys.command`、`exit` |
| `wildcard` | 207 | 何でも受ける match の枝。<code>&#124; _ -></code> |
| `strings` | 84 | 型の定義の中で、欄や引数に `string` を使っている箇所。はじめは 240 と数えていた。関数の引数や局所変数の型注釈まで数えていたためである。R8 のときに、型の定義の中だけを数えるように直した |

数え方は正規表現である。厳密な構文解析ではない。同じ数え方で前後を比べるための数であり、絶対値に意味を持たせすぎない。

入出力は、すでに端に寄っている。56 箇所のうち 40 箇所が `lib/CliEngine.ml` にある。残りは `lib/Util.ml` に 6、`lib/Cli.ml` に 4、`lib/Compiler.ml` に 4、`lib/HtmlError.ml` と `lib/Reporter.ml` に 1 ずつである。

例外を投げる 476 箇所の内訳は、次のとおりである。

| 仕組み | 数 |
|---|---|
| `austral_raise`。文面を作って投げる箇所。うち1つは `austral_raise` 自身の定義である。これを呼ぶ関数は 140 個で、エラーの種類の数に当たる | 145 |
| `Errors.x`。その関数を呼ぶ箇所 | 184 |
| `internal_err` | 93 |
| `err "..."` | 43 |
| `raise` | 11 |

合計は 476 で、表の `raise` と一致する。

`internal_err` が多いのは、`lib/Monomorphize.ml`（13）、`lib/TypeStripping.ml`（12）、`lib/Env.ml`（12）、`lib/CodeGen.ml`（10）、`lib/LinearityCheck.ml`（9）である。

例外を投げる箇所が多いのは、`lib/TypeErrors.ml`（55）、`lib/TypeCheckExpr.ml`（50）、`lib/TypingPass.ml`（34）、`lib/CombiningPass.ml`（28）、`lib/TypeMatch.ml`（26）である。

### 例外を投げうる関数（`raising-functions.py`）

最上位の関数は 803 個ある。本体に例外を投げる箇所を持つものが 220 個。それを呼ぶものまで含めると 414 個である。
関数の名前を、ファイルをまたいで単語として探した。同じ名前の関数が別のモジュールにあると、区別できない。414 は上限の見積もりである。
計画の R12 が、この数を使う。失敗しうる関数のすべてに `result` を返させると、およそ半分の関数の型が変わる。

### 壊れやすい match（`measure.sh --fragile`）

OCaml のコンパイラは、何でも受ける枝を持つ match のうち、型に構成子を足すと黙って飲み込むものを、警告 4 で報告する。コンパイラ全体をこの警告を有効にしてビルドし、数えた。sexp の導出が生成したコードの分は除いた。

| 型 | 数 |
|---|---|
| `Type.ty` | 152 |
| `EnvTypes.decl` | 76 |
| `EnvTypes.monomorph` | 16 |
| `Cst.concrete_def` | 12 |
| `Stages.Tast.texpr` | 12 |
| `Type.universe` | 12 |
| `MonoType.mono_ty` | 10 |
| その他 | 108 |
| 合計 | 398 |

`Type.ty` に `Decimal` の構成子を足すとき、コンパイラが教えてくれない箇所が 152 ある、という意味である。

sexp の導出が生成したコードには、別に 52 個ある。11 ファイルにあり、`lib/Type.ml` が 16 個である。27 個の型定義が sexp との変換を導出しているが、その変換を呼ぶコードは、どこにもない。導出を消せば、この 52 個もなくなる。

## 再実行のしかた

```
measure.sh                 # 数を expected/ と比べる
measure.sh --fragile       # 壊れやすい match も数える。ビルドが要る
unused-exports.sh          # 外から使われていない公開関数を、expected/ と比べる
coverage.sh                # カバレッジを expected/ と比べる。ビルドが要る
```

ビルドの要るものは、nix の入力を取得できない環境では `ZUSTRAL_NIX_FLAGS` に `--override-input` を渡す（issue #42）。

## お手本

Scott Wlaschin の "Domain Modeling Made Functional" のコード例を読んだ。`https://github.com/swlaschin/DomainModelingMadeFunctional` のコミット `8153616` である。計画が参照するファイルは、どれも `src/OrderTaking/` にある。

| ファイル | 示していること |
|---|---|
| `Common.SimpleTypes.fs` | 制約のある単純な型。構成子を隠し、`create` で1回だけ検査する |
| `Common.CompoundTypes.fs` | 単純な型を組んだ複合の型 |
| `PlaceOrder.PublicTypes.fs` | 作業の流れの入力、出力、エラーの型。流れそのものを関数の型で書く |
| `PlaceOrder.Implementation.fs` | 各段を関数の型として先に定義する。依存は引数に書く。状態ごとに別の型（未検証、検証済み、価格付き）。最後に段をつなぐ |
| `Result.fs` | エラーを値として運ぶ `Result`。複数のエラーを集める `Validation` |
| `PlaceOrder.Dto.fs`、`PlaceOrder.Api.fs` | 外の形と中の型の変換を、端に置く。入出力は端だけで行う |

## 後続の項目のための棚卸し

`inventory-c7af9a6.md` は、コミット `c7af9a6` の時点で、後ろの項目が直す箇所を数え上げたものである。行番号は、そのコミットのものである。

| 節 | 使う項目 | 中身 |
|---|---|---|
| 1 | R21 | `lib/Env.ml` の外から、環境を検索する 43 箇所。キーが利用者の書いた名前か id か、見つからないときに何をするか |
| 2 | R24 | `internal_err` の 91 箇所と、原因の分類 |
| 3 | R25 | 宣言、式、文の型の match のうち、何でも受ける枝を持つ 78 箇所 |

計画の数と合わないところがある。R21 の内訳は、計画が 27、8、4、1、4（合計 44）、棚卸しが 29、7、2、1、4（合計 43）である。内部エラーのうち 5 箇所は、利用者のプログラムで届く（issue #128、#91）。R21 と R24 の目標の数は、これを踏まえて読む。

