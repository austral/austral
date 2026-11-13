# 関数型のドメインモデリングによるリファクタリングの計画

コンパイラ（`lib/` と `bin/`、OCaml、手で書いた分が約 15000 行）を、不具合を直す前に作り直す計画である。
お手本は Scott Wlaschin の "Domain Modeling Made Functional" と、そのコード例 `swlaschin/DomainModelingMadeFunctional` である。

**守る規則は1つである。構造は変える。振る舞いは変えない。**
今ある不具合も、そのまま残す。不具合は、作り直した後の形の上で直す。そのほうが、1件ごとの修正が小さくなる。

この文書は、何をどの順で変えるかと、変えていないことの確かめ方を書く。
今の形を測った記録は `docs/research/functional-domain-refactoring/` にある。
全体の順序は `docs/plan.md` にあり、この計画はその中の R の段である。進み具合は issue #84 で追う。

**状態。** 計画だけである。どの項目にも、まだ着手していない。

## 1. 読み方

1つの項目は、1つの変更である。1つの変更は、1つのプルリクエストにする。大きい項目は、手順に分けてある。手順ごとに1つのプルリクエストにする。

各項目に、次のことを書く。

- **依存。** 先に終わっている必要がある項目。
- **今の形。** 測った数か、ファイルと行。
- **目標の形。** 変えた後の型と、関数の形。
- **確かめ方。** 振る舞いが変わっていないことと、形が良くなったことを、それぞれ何で見るか。
- **直しやすくなる issue。** この項目の後で、修正が小さくなる不具合。

大きさは、小（半日以内）、中（1日から2日）、大（数日）で書く。どれも推測である。

## 2. すべての項目に共通の規則

### 2.1 振る舞いが変わっていないことを、3つで確かめる

型検査が通ることと、テストが通ることだけでは、確かめたことにならない。

1. **等価性の検査。** 変更の前後のコンパイラで、リポジトリにあるすべてのプログラムをコンパイルし、生成した C、標準出力、標準エラー、終了コードを比べる。1本でも違えば、その変更は振る舞いを変えている。いまは 308 本を 12 秒で比べられる（`docs/research/functional-domain-refactoring/equivalence.py`）。
2. **テスト。** end-to-end のスイート全体と `dune runtest` が通る。
3. **動かす前に固定する。** 等価性の検査が見るのは、308 本が通る道だけである。今のテストは、手で書いたコードの 77.33% しか実行していない。項目が触る関数のうち、テストが実行していない枝には、構造を動かす前にテストを足す。足したテストは、変更の前のコンパイラで通ることを先に見る。

### 2.2 形が良くなったことを、数で確かめる

`docs/research/functional-domain-refactoring/measure.sh` が、例外を投げる箇所、可変の状態、入出力、何でも受ける match の枝を、ファイルごとに数える。数は `expected/` に記録してある。

- どの項目も、これらの数を増やしてはいけない。
- 各項目は、減らす数を名指しする。終わったら、記録を新しい数で書き直す。
- 数え方は正規表現である。数を減らすために書き方だけを変えることは、しない。

### 2.3 引いてから足す

新しい形を入れる前に、使われていないコード、呼び出しが1つしかない包み、同じ検査の重複を消す。

### 2.4 古い形を残さない

新しい関数の形を入れたら、同じプルリクエストの列の中で、呼び出す側をすべて移し、古い関数を消す。新旧の両方が動く期間を作らない。
例外が1つある。段の境界でエラーを値に変える項目（R13）は、段ごとに進める。その間、まだ移していない段は例外を投げる。境界の関数が例外を値に変える。この変換は、最後の段を移したときに消す。

### 2.5 見つけた不具合は、直さずに issue にする

作り直す途中で不具合を見つけたら、今の振る舞いを固定するテストを書き、issue を立てる。その場では直さない。

## 3. お手本と、このコンパイラへの写し方

お手本のコードは、コミット `8153616` の `src/OrderTaking/` を読んだ。

| お手本の方法 | お手本のファイル | このコンパイラの今 | 項目 |
|---|---|---|---|
| 作業の流れを、入力から出力への関数の型で書く。`PlaceOrder = UnvalidatedOrder -> AsyncResult<PlaceOrderEvent list, PlaceOrderError>` | `PlaceOrder.PublicTypes.fs` の 117 行から | 流れは `lib/Compiler.ml` の `compile_mod` にある。型は `compiler -> module_source -> compiler` で、出力は C の文字列の連結である。失敗は例外で出る | R10、R13、R27 |
| 段ごとに、関数の型を先に定義する。依存は引数に書く | `PlaceOrder.Implementation.fs` の Section 1 | 段の関数はある。型は各モジュールの `.mli` にばらばらにある。依存のうち、id の採番と呼び出しの記録は、大域の可変の状態である | R16、R9、R27 |
| 状態ごとに別の型にする。未検証、検証済み、価格付き | `UnvalidatedOrder`、`ValidatedOrder`、`PricedOrder` | すでにある。`Cst`、`Ast`、`Combined`、`Linked`、`Tast`、`Mtast`、`CRepr` と、段ごとに型が分かれている。ただし、検査だけして同じ型を返す段がある | R20 から R23 |
| 不正な状態を、型で表せなくする | `Common.SimpleTypes.fs` と `Common.CompoundTypes.fs` | 後ろの段が、前の段で弾くべきものを受け取って、例外を投げている。`lib/CodeGen.ml` の外部関数の型の検査がその例である | R20 から R23 |
| 制約のある単純な型。構成子を隠し、`create` で1回だけ検査する | `Common.SimpleTypes.fs` の `String50`、`ConstrainedType` | `identifier`、`module_name`、`qident`、`env` は、すでに抽象型である。id は構成子が見えている。C の識別子と C のコード片は `string` である | R16 から R19 |
| エラーを値で運ぶ。段のエラーを、流れのエラーの直和にまとめる | `Result.fs`、`PlaceOrderError` | エラーは1種類の例外 `Austral_error` で、中身は文面である。投げる箇所が 476、捕まえる箇所が 56 ある | R11 から R15 |
| 複数のエラーを集める | `Result.fs` の `Validation` | 最初のエラーで止まる | R13 の後で issue #57 |
| 入出力を端に置く。核は純粋な関数にする | `PlaceOrder.Api.fs` | 入出力の 56 箇所のうち 40 箇所は、すでに `lib/CliEngine.ml` にある。残りが核に混ざっている。`lib/Compiler.ml` は、モジュールを読み込んだ時点で組み込みモジュールをコンパイルし、失敗すると `exit` する | R8、R9 |
| 外の形と中の型の変換を、端に置く | `PlaceOrder.Dto.fs` | 外の形は、コマンドラインの引数と、エラーの JSON の2つだけである。引数の解析は `lib/CliParser.ml` にある | R11 |
| すべての場合を match で受ける | F# の網羅性の検査 | 新しい構成子を黙って飲み込む match が 398 ある | R24、R25 |

## 4. 目標の形

作り直した後の形を、先に書く。項目は、この形へ向かう。

### 4.1 コンパイルの流れは、1つの純粋な関数である

```ocaml
(* 失敗は値である。エラーは1件とは限らない *)
type 'a outcome = ('a, Diagnostic.t list) result

(* 流れの全体。ファイルも、画面も、プロセスも触らない *)
type compile = source list -> entrypoint option -> c_program outcome
```

`source` は、ファイル名と中身の組である。ファイルを読むのは、この関数を呼ぶ側である。
`c_program` は、C の宣言の列である。文字列にするのは、最後の1回だけである。

### 4.2 段は、関数の型である

```ocaml
type parse           = source -> parsed_module outcome
type combine         = env -> parsed_module -> combined_module outcome
type desugar         = combined_module -> small_combined_module
type extract         = env -> small_combined_module -> (env * linked_module) outcome
type check_types     = env -> linked_module -> typed_module outcome
type check_linearity = typed_module -> checked_module outcome
type monomorphize    = env -> checked_module -> (env * mono_module) outcome
type generate        = env -> mono_module -> c_unit
```

- `outcome` のない段は、失敗しない。`desugar` と `generate` がそうである。今の `lib/CodeGen.ml` には、例外を投げる箇所が 16 ある。目標は 0 である。前の段の型が、不正な入力を表せなくする。
- `checked_module` は、線形性の検査を通ったことを表す型である。今は、検査の後も `typed_module` のままである。検査を通していないモジュールを、後ろの段へ渡せてしまう。
- `env` は、今も値として受け渡している。id の採番だけが、大域の状態に残っている。それも `env` に入れる。

### 4.3 エラーは、構造のある値である

```ocaml
type location =
  | At of { module_name : module_name; span : span }
  | In_module of module_name     (* 位置を持たないエラー。今の「no span available」 *)
  | Nowhere                      (* コマンドラインのエラー *)

type error =
  | Syntax of Syntax_error.t
  | Declaration of Declaration_error.t
  | Type of Type_error.t
  | Linearity of Linearity_error.t
  | Entrypoint of Entrypoint_error.t
  | Internal of string           (* コンパイラの不具合 *)

type t = { location : location; error : error }

val render : t -> ErrorText.err_text   (* 純粋。文面は今と同じ *)
```

今の `lib/TypeErrors.ml` は、文面を作って例外を投げる関数が 55 個並んだファイルである。これが、55 個の構成子を持つ `Type_error.t` と、1つの `render` になる。テストは、文面ではなく構成子を比べられる。

### 4.4 入出力は、端だけにある

```ocaml
(* bin/ と、コマンドラインのモジュールだけが持つ *)
val read_sources : command -> source list outcome   (* ファイルを読む *)
val write_output : command -> c_program -> unit     (* C を書く。cc を呼ぶ *)
val report       : command -> Diagnostic.t list -> unit
val exit_code    : 'a outcome -> int
```

核は、`unix` に依存しない別のライブラリにする。核が入出力をしていないことを、ビルドが確かめる。

### 4.5 型検査は、推論と検査の2つの関数である

```ocaml
val infer : ctx -> aexpr -> texpr          (* 式から型を求める *)
val check : ctx -> aexpr -> ty -> texpr    (* 式が、与えた型を持つことを確かめる *)
```

今の `augment_expr` は、`asserted_ty: ty option` という省略できる引数で、この2つを1つの関数にしている。引数は、呼び出し、変数、算術には渡り、比較、`if` 式、否定には渡らない（`lib/TypeCheckExpr.ml` の 521 行から）。
`docs/decimal.md` の4節の「型が届く位置」は、`check` を呼ぶ位置のことである。

## 5. お手本のうち、当てはめないもの

- **境界づけられたコンテキストと、その間の変換。** お手本は、複数のチームと複数のシステムを想定している。コンパイラは1つのプログラムで、段がその役を果たす。段の間に、変換の層は足さない。
- **保存と、保存のための形。** コンパイラは何も保存しない。
- **非同期。** お手本の `AsyncResult` に当たるものは要らない。並行に動くものがない。
- **依存をすべて引数にすること。** 引数にするのは、副作用のある依存だけである。組み込みモジュールのソースと、C の前置きは、定数のままにする。
- **イベントの列を返すこと。** コンパイルの出力は、C のプログラムと、エラーの列である。

## 6. 今の形を測った結果

数え方と再実行のしかたは、`docs/research/functional-domain-refactoring/README.md` にある。

| 指標 | 今 | 目標 | 減らす項目 |
|---|---|---|---|
| 例外を投げる箇所 | 476 | 段の内側だけ。段の外へ出る例外は 0 | R11、R13、R15 |
| 例外を捕まえる箇所 | 56 | 段の境界の数だけ | R13、R14 |
| 可変の状態 | 13 | 核では 0 | R9、R16 |
| 核の中の入出力 | 16 | 0 | R8、R9 |
| 何でも受ける match の枝 | 207 | ドメインの型については 0 | R24、R25 |
| 壊れやすい match | 398 | ドメインの型については 0 | R24、R25 |
| 外から使われていない公開関数 | 68（346 個中） | 0 | R7 |
| テストが実行するコード | 77.33% | 項目が触る関数については 100% | R2、R3、R4 と、各項目の最初の手順 |
| `lib/TypeErrors.ml` のうち、テストが実行する部分 | 20.69% | 100% | R3 |
| `lib/CodeGen.ml` の例外を投げる箇所 | 16 | 0 | R20、R21 |
