# 型に値パラメータを足す道筋（issue #2）

`Decimal[7, 2]` のように型が自然数の引数を取れるようにすると、Zustral のコンパイラのどこが変わるかを調べたレポートである。
比べるために、スケールを値に持たせて型には入れない道も、同じ方法で測った。

このレポートはどちらの道も選ばない。
選ぶのは issue #4（0004「スケールは型に入るか、値に入るか」）の仕事である。
ここでは、変更が届く範囲の形と、それが下流の問いに与える事実を並べるところまでにとどめる。

## 読み方

事実を述べる文には、次のどれかの根拠を付ける。

- `〔spike: <記録> / <行>〕` は `expected/<記録>.txt` の行を指す。`spike.sh` が再実行して照合する。
- `〔src: <ファイル>:<行>〕` は、コミット f601bcb の `lib/` 以下のソースを指す。
- `〔推論〕` は、ソースを読んで導いたが、実行しては確かめていない主張に付ける。

## 再実行の方法

```sh
docs/research/value-parameter-generics/spike.sh            # expected/ と照合する
docs/research/value-parameter-generics/spike.sh --record   # expected/ を書き直す
```

`spike.sh` は HEAD のコピーを一時ディレクトリに取り出し、リポジトリの flake の環境（`nix develop`）でビルドする。
作業ツリーには何も書かない。
4つの記録を照合し、最後に `match 4 / mismatch 0 / skipped 0` の形で結果を出す。
このマシン（x86_64 Linux）では4つとも一致した。

環境変数は2つある。

- `ZUSTRAL_BUILD_TIMEOUT` はビルド1回の上限の秒数で、既定は 900 である。
- `ZUSTRAL_NIX_FLAGS` は `nix develop` に足すフラグである。この調査を行ったクラウド環境からは GitHub の API に届かず、flake の入力を取得できなかった。そこで `flake.lock` と同じリビジョンを git で取得し、`--override-input nixpkgs path:<dir>` のように差し替えて実行した。

## 測り方

### 構成子を足すだけのパッチ

`patches/` の2つのパッチは、コンパイラの型の定義に構成子を足す。
それを扱うコードは1行も書かない。
するとコンパイラ自身が、直すべき場所を警告として列挙する。

- 警告 8（partial-match）は、網羅性が崩れた `match` を指す。コンパイラが必ず直させる箇所である。「マッチしない例」に新しい構成子が出たものだけを数える。
- 警告 4（fragile-match）は、ワイルドカード `_` が新しい構成子まで黙って吸い込む `match` を指す。コンパイラは何も言わないが、意味を決め直すことになるかもしれない箇所である。パッチで足した型の上の `match` だけを数える。

警告は `OCAMLPARAM="_,w=+4+8,warn-error=-a"` で有効にし、エラーにはしない。
だからビルドは最後まで進み、全箇所が一度に出る。
ppx の `deriving` が生成するコードは新しい構成子を自動で扱うので、数に入らない。

### 3つの案

| 案 | 中身 | 足した構成子 |
|---|---|---|
| 案A 不透明な組み込み型 | `Decimal` を `Pervasive` の不透明な型として宣言する。スケールは値に持たせる | なし。測っていない |
| 案B 組み込みの原始型（`primitive-decimal`） | `Float64` と同じく `ty` の構成子にする。スケールは値に持たせる | `Type.ty` に `DecimalTy`、`MonoType.mono_ty` に `MonoDecimal` |
| 案C 値パラメータ（`value-parameters`） | 型が自然数の引数を取れるようにする | `Type.ty` に `TyNat of int`、`Type.universe` に `NaturalUniverse`、`MonoType.mono_ty` に `MonoNat of int`、`Cst.typespec` に `ConcreteNatSpec of string`、`Stages.Ast.qtypespec` に `QNatSpec of string` |

案C は値パラメータの仕組みだけを足し、`Decimal` 型そのものは含まない。
`Decimal[P, S]` を作るには、案C に加えて、案A か案B の形で `Decimal` を置くことになる。

### 実際に動かした2つの検査

- `mono-c` は、`probes/Mono.aum` を HEAD のコンパイラで C に変換し、gcc でビルドして実行する。汎用の型と関数が、型引数ごとに C でどう出るかを数える。
- `grammar` は、`lib/Parser.mly` の型引数の位置を広げて menhir の LR 構成を走らせ、文法の衝突を数える。`patches/grammar-literal.patch` は整数リテラルだけを許し、`patches/grammar-expression.patch` は式を許す。どちらも `value-parameters.patch` の上に当てる。

## 結果の数

| | 案B | 案C |
|---|---|---|
| コンパイラが直させる関数（警告 8） | 17 | 27 |
| ワイルドカードが吸い込む `match`（警告 4） | 81 | 87 |
| 触れるファイル | 15 | 19 |
| 触れる関数（警告 4 と 8 の合計） | 42 | 56 |

根拠は 〔spike: primitive-decimal / partial-matches〕 〔spike: primitive-decimal / fragile-matches〕 〔spike: value-parameters / partial-matches〕 〔spike: value-parameters / fragile-matches〕 である。

案B が直させる17関数は、すべて案C の27関数に含まれる 〔spike: primitive-decimal / sites〕 〔spike: value-parameters / sites〕。
どちらも `ty` と `mono_ty` に構成子を1つずつ足すので、同じ17関数が反応する。
`equal_ty`、`match_type`、`replace_variables`、`type_universe`、`is_numeric`、`strip_type'`、`mono_to_ty`、`gen_type` などである。

案C で増える10関数は、2か所に集まる。

- 種の扱いが7関数ある。`Type.universe_string`、`TypeBindings.universe_compatible`、`TypeParser.universe_compatible`、`TypeParser.effective_universe`、`ExtractionPass` の `check_slots_are_free`、`check_typarams_are_free`、`extract_definition` である。今のコンパイラで型パラメータの種にあたるものは `universe`（Free、Linear、Type、Region）しかない。
- 構文の扱いが3関数ある。`Qualifier.qualify_typespec`、`CstUtil.typespec_string`、`TypeParser.parse_type` である。

警告 4 の差は6箇所で、どれも `Type.universe` の上の `match` である 〔spike: value-parameters / sites〕。

機械的に数えると、値パラメータの仕組みは「`ty` に構成子を1つ足すのと同じ17関数」に「種と構文の10関数」を足した大きさになる。
ただし、この数はどの関数に触れるかであって、各関数で何をするかではない。
重い部分は次の節にある。

## 数に出ない変更

### 型レベルの算術

`Decimal[P1, S1] * Decimal[P2, S2]` の結果を `Decimal[P1+P2, S1+S2]` と書くには、型の中に `P1+P2` のような式が要る。

今の `equal_ty` は構造の比較である 〔src: Type.ml:133〕。
`NamedType` どうしは、名前と各引数と universe を順に比べる 〔src: Type.ml:165-172〕。
このままでは `3+4` と `7` は等しくならない。
式を正規形に直す処理が、`equal_ty` と `match_type` 〔src: TypeMatch.ml:54〕 に要り、`replace_variables` で代入した後にも要る 〔src: TypeBindings.ml:139〕 〔推論〕。

`match_type` は片方向の照合で、呼ばれる側の型にある `TyVar` だけが束縛を作る 〔src: TypeMatch.ml:142〕。
引数の型に `N+1` を書けるようにすると、照合は「`N+1` と `7` から N を解く」問題になる。
式を戻り値の型だけに許すなら、代入してから正規化すれば済む 〔推論〕。

この重さは警告の数に出ない。
式の構成子（たとえば `TyNatAdd of ty * ty`）を足しても、反応する関数は `TyNat` と同じ集合になり、違うのは各関数の中身である 〔推論〕。

### 算術演算子と型クラス

演算子 `+ - * /` の型検査は、両辺の型が OCaml の `=` で等しく、かつ `is_numeric` が真であることを求める 〔src: TypeCheckExpr.ml:938-944〕。
`is_numeric` は `NamedType` に偽を返す 〔src: TypeSystem.ml:30-36〕。
演算子は型クラス `TrappingArithmetic(T)` のメソッドに変換され、その型は `(lhs: T, rhs: T): T` である 〔src: builtin/Pervasive.aui:65-69〕。

だから `Decimal[7,2] * Decimal[5,1]` は、今の規則では3つの理由で通らない。

- 両辺の型が違う。
- `NamedType` は数値型ではない。
- 型クラスが、結果の型を両辺と同じ型に縛る。

案B では Decimal の値がすべて同じ型なので、`is_numeric` に1行足して `TrappingArithmetic` のインスタンスを置けば、`(T, T): T` の形のまま演算子が通る 〔推論〕。
案C でスケールの違う値を演算子で混ぜるには、Decimal 専用の型付け規則を置くか、型クラスに複数の型パラメータか関連型を足すことになる 〔推論〕。

### 型引数ごとに複製される C の型と関数

単相化は、宣言と型引数の組ごとに新しい番号を振る 〔src: Env.ml:432-458〕。
C の名前は `mono_N` で、型引数は名前に入らない 〔src: CodeGen.ml:101-103〕。

`probes/Mono.aum` は、`Int64` の欄を1つだけ持つ汎用のレコード `Tagged[T]` を、`Tagged[Int8]` と `Tagged[Int16]` で使う。
C には中身が同じ struct が2つ出た 〔spike: mono-c / structs-with-only-units 2〕。
それを受ける汎用関数 `raw` も2つ出た 〔spike: mono-c / function-monomorphs-of-raw 2〕。
プログラムは終了コード 0 で終わった 〔spike: mono-c / exit 0〕。

案C で `Decimal[P, S]` をレコードとして書くと、`Decimal[7,2]` と `Decimal[9,2]` は同じ16バイトの表現でも別の struct になる。
それを受ける汎用関数も、(P, S) の組ごとに複製される 〔推論〕。
1つの C 型を共有する先例は `Span` で、どの `T` でも `au_span_t` になる 〔src: CodeGen.ml:147-150〕。
Decimal を同じ扱いにするには、案B と同じく `ty` に専用の構成子を足すことになる 〔推論〕。

### 型引数を値として読む

汎用関数の本体が型引数を値として使える道は、今は `sizeof(T)` だけである 〔src: TypeReplace.ml:99-101〕。
本体に裸の `S` を書くと、変数の解決は関数、定数、引数、局所変数しか探さず、未知の名前として失敗する 〔src: TypeCheckExpr.ml:570-605〕。

Decimal の演算は、桁合わせや丸めのために実行時にスケールを知る必要がある。
案C で本体がスケールを読むには、3つの部品が新しく要る 〔推論〕。

- 型引数を値として読む式の構成子
- その名前を型パラメータへ解決する型検査
- 単相化のときに、それを整数定数へ置き換える処理

案A と案B では、スケールは値の欄なので、今の仕組みのまま読める。

### 型引数の構文

型引数に整数リテラルだけを許す文法では、menhir の衝突は増えなかった。
HEAD と同じく、`TRIPLE_STRING_CONSTANT` での shift/reduce が2件だけである 〔spike: grammar / literal〕。

型引数に式を許すと、`]` での shift/reduce が1件、`]` と `,` での reduce/reduce が1件増えた 〔spike: grammar / expression〕。
識別子1つが、型としても変数としても読めるからである 〔推論〕。
だから `Decimal[P1+P2, S1+S2]` のような式は、今の型の文法には足せない。
式を書くなら、別の区切り記号か、型の位置だけで使う小さな式の文法が要る 〔推論〕。

字句解析は `-1` を1つの整数リテラルとして読む 〔src: Lexer.mll:77, 85〕。
自然数の引数では、負の数をどこかで弾く必要がある。

### インターフェースと本体の照合

不透明な型の宣言では、インターフェースと本体の型パラメータを、CST のまま構造で比べる 〔src: CombiningPass.ml:166-201〕。
リテラルを位置情報付きの値で持つと、同じ `7` でも位置が違うので等しくならない。
型引数のリテラルは、位置情報を持たない正規化した値として持つ必要がある 〔推論〕。

### 型クラスのインスタンス

`instance Printable(Decimal[P, S])` は、P と S が型変数として読まれれば、今の「引数はすべて相異なる型変数」という検査を通る 〔src: TypeClasses.ml:92-122〕 〔推論〕。
インスタンスの重なりの検査は型の名前しか比べない 〔src: TypeClasses.ml:184-189〕。
そのため `Decimal[7,2]` だけのインスタンスは作れず、1つの型クラスにつき Decimal のインスタンスは1つになる。

## スケールを値に持たせる道との比較

| 観点 | 案A 不透明型 | 案B 原始型 | 案C 値パラメータ |
|---|---|---|---|
| 型の定義の変更 | なし | `ty` と `mono_ty` に1つずつ | 5つの型に1つずつ |
| コンパイラが直させる関数 | 0（構成子を足さない） | 17 | 27。Decimal 自体を案B の形で置くなら、その17関数と重なる |
| 演算子 `+ - *` | 使えない。`is_numeric` が偽を返す | `is_numeric` とインスタンスを足せば通る | 同じスケールどうしは案B と同じ。違うスケールどうしは専用の型付け規則か型クラスの拡張が要る |
| スケールの違う値の演算 | 実行時に揃える | 実行時に揃える | 結果のスケールを型で決めるには、型レベルの算術が要る |
| C の表現 | レコードなら struct は1つ | C の型は1つ | レコードなら (P, S) ごとに struct と関数が複製される。専用の構成子なら1つ |
| 本体でスケールを読む | 値の欄を読む | 値の欄を読む | 新しい式の仕組みが要る |
| 型が保証すること | なし。実行時に検査する | なし。実行時に検査する | 精度とスケールの一致 |

## 下流の問いに効く事実

### 0004「スケールは型に入るか、値に入るか」

値パラメータの仕組みだけなら、機械的な変更は、`ty` に構成子を1つ足すのと同じ17関数に、種と構文の10関数を足した範囲にとどまる。
重いのは数に出ない3つである。型レベルの算術とその正規形、演算子の型付け規則、本体から型引数を読む仕組みである。

間に、もう1つ道がある。
型引数を整数リテラルだけに限り、型レベルの算術を持たない道である。
この道では、同じ (P, S) どうしの `+` と `-` は型で検査できる。
`*` と `/` は、結果の型を呼び出し側が書く。
Austral には、戻り値の型にだけ出る型変数を、呼び出し側の型注釈で決める仕組みがすでにある 〔src: TypeCheckExpr.ml:481-497〕。
ただし、本体がその型引数を値として読めないと、関数は結果のスケールに合わせた計算ができない。

### 0006「除算のスケールを誰が決めるか」

上の「リテラルだけ」の道を取ると、「呼び出し側が型注釈で結果のスケールを決める」という答えを、今の型の仕組みで書ける。
そのためには、本体が型引数を値として読む仕組みが前提になる。

### 0009「Decimal の算術を演算子で書けるようにするか」

演算子は `(T, T): T` の型クラスに変換されるので、案A と案B のように Decimal が1つの型なら、両辺と結果は同じ型になる。
案C でスケールの違う値を演算子で混ぜるには、この形を崩す必要がある。

## 未確認事項

- 型レベルの算術の正規形を実装する手間は測っていない。数はどの関数に触れるかであって、中身の量ではない。
- 警告 4 の 81〜87 箇所のうち、本当に意味を変える必要があるものは数えていない。多くは `NamedType` や `Integer` だけを特別に扱い、残りを `_` に流す形をしている。新しい構成子を「その他」として扱えば済む箇所も多いと見ているが、確かめていない 〔推論〕。
- 測ったのは `lib/` だけである。`test/` の OCaml のテストには `ty` の構成子を `match` する箇所がない。`bin/` は確かめていない。
- 文法の検査で変えたのは型引数の位置だけである。型パラメータの宣言 `generic [P: Natural]` の文法は試していない。`Natural` を universe と同じキーワードにするなら、新しいトークンが要る 〔src: Lexer.mll:160-163〕。

## ファイル

- `spike.sh` が4つの検査を再実行し、`expected/` と照合する。
- `patches/primitive-decimal.patch` と `patches/value-parameters.patch` は、型の定義に構成子を足すだけのパッチである。
- `patches/grammar-literal.patch` と `patches/grammar-expression.patch` は、型引数の文法を広げるパッチである。`value-parameters.patch` の上に当てる。
- `probes/Mono.aum` は、`mono-c` の検査で C に変換する Zustral のプログラムである。
- `expected/` に4つの検査の記録がある。
