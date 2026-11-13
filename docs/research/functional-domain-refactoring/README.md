# リファクタリングの前に測ったこと

`docs/refactoring-plan.md` の根拠である。コンパイラ（`lib/` と `bin/`）を、関数型のドメインモデリングの方法で作り直す前に、今の形を数え、振る舞いを固定する手段を試した。

ここにあるスクリプトは、どれも再実行できる。数は `expected/` に記録してあり、スクリプトは今の木と比べて違いを報告する。

対象のコミットは `f408e59` である。

## 何を確かめたか

| 問い | 答え | 確かめ方 |
|---|---|---|
| リファクタリングの前後で、コンパイラの出力が同じだと確かめられるか | 確かめられる。308 本のプログラムについて、生成した C、標準出力、標準エラー、終了コードを記録して比べる。12 秒で終わる | `equivalence.py` |
| 生成した C は、実行のたびに同じか | 同じである。同じコンパイラで2回記録して、308 本すべてが一致した | `equivalence.py` を2回 |
| その比較は、違いを見つけるか | 見つける。issue #16 と #22 を直す前のコンパイラと比べると、22 本が違った。直した規則を試すテストの 22 本と一致する | `equivalence.py compare` |
| 今のテストは、コンパイラのどれだけを実行しているか | 手で書いたコードの 77.33%。エラーを作る `TypeErrors.ml` は 20.69% | `coverage.sh` |
| 例外、可変の状態、入出力は、どこにどれだけあるか | 下の表 | `measure.sh` |
| 新しい構成子を黙って飲み込む match は、いくつあるか | 398 個、27 ファイル | `measure.sh --fragile` |

## 等価性の検査（`equivalence.py`）

```
python3 equivalence.py snapshot ./austral OUT_A
python3 equivalence.py snapshot path/to/other/austral OUT_B
python3 equivalence.py compare OUT_A OUT_B
```

- 対象は、end-to-end のテスト 307 本と、標準ライブラリ（テストを含む）1 本である。
- プログラムごとに、終了コード、生成した C、標準出力、標準エラーを保存する。パスは `$CWD` と `$ROOT` に置き換える。
- コンパイラは、プログラムごとの一時ディレクトリで動かす。失敗したときに、作業ディレクトリへ `error.html` と `calltree.html` を書くためである。
- 308 本のうち、C が出るのは 93 本である。215 本は、コンパイルエラーを期待するテストである。エラーの文面も比べるので、エラーの出し方を変えると検出される。

**限界。** この検査が固定するのは、308 本が通る道だけである。テストが実行しないコードを動かしても、検査は何も言わない。次の節の数が、その大きさである。

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
| `catch` | 56 | 例外を捕まえるか、位置を付け直す箇所。`try`、`adorn_error_with_*` |
| `state` | 32 | 可変の状態。`ref`、`Hashtbl`、`mutable` |
| `io` | 56 | 入出力とプロセスの制御。ファイル、表示、`Sys.command`、`exit` |
| `wildcard` | 207 | 何でも受ける match の枝。<code>&#124; _ -></code> |
| `strings` | 240 | 型の定義の中で、欄や引数に `string` を使っている箇所 |

数え方は正規表現である。厳密な構文解析ではない。同じ数え方で前後を比べるための数であり、絶対値に意味を持たせすぎない。

入出力は、すでに端に寄っている。56 箇所のうち 40 箇所が `lib/CliEngine.ml` にある。残りは `lib/Util.ml` に 6、`lib/Cli.ml` に 4、`lib/Compiler.ml` に 4、`lib/HtmlError.ml` と `lib/Reporter.ml` に 1 ずつである。

例外を投げる箇所が多いのは、`lib/TypeErrors.ml`（55）、`lib/TypeCheckExpr.ml`（50）、`lib/TypingPass.ml`（34）、`lib/CombiningPass.ml`（28）、`lib/TypeMatch.ml`（26）である。

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

## 再実行のしかた

```
measure.sh                 # 数を expected/ と比べる
measure.sh --fragile       # 壊れやすい match も数える。ビルドが要る
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
