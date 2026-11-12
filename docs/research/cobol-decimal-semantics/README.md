# COBOL の10進意味論から何を継ぐか（issue #3）

COBOL が10進計算について定めている規則を、Zustral の Decimal の設計材料として並べたレポートである。
対象は、`PICTURE` による桁とスケールの宣言、中間結果の精度、`ROUNDED` と丸めモード、`ON SIZE ERROR`、`GIVING` と `MOVE` の暗黙の切り捨て、符号の消失である。
あわせて、これらの規則が実際に起こした不具合を IBM の APAR（コンパイラの不具合報告）から拾った。

このレポートは何を継ぐかを決めない。
決めるのは issue #4（0004）、#6（0006）、#7（0007）、#8（0008）、#12（0009）の仕事である。
ここでは、各問いに効く事実を名指しし、継ぐ候補と避ける候補を挙げるところまでにとどめる。

## 読み方

事実を述べる文には、次のどれかの根拠を付ける。

- `〔probe: <記録> / <行>〕` は、`expected/<記録>.txt` の行を指す。`run.sh` が GnuCOBOL 3.1.2 で再実行して照合する。GnuCOBOL という1つの処理系での観測であり、規格の規定ではない。
- `〔src: <ID>〕` は、`sources.tsv` の行を指す。行には URL と、文書から写した引用がある。`check-sources.sh` が、引用がいまも文書にあるかを確かめる。
- `〔推論〕` は、上の根拠から導いたが、直接は確かめていない主張に付ける。

主な文書は次の4つである。

- IBM Enterprise COBOL for z/OS 6.4 Language Reference（SC27-8713-03）。以下 IBM LR と書く。
- IBM Enterprise COBOL for z/OS 6.4 Programming Guide（SC27-8714-03）。以下 IBM PG と書く。
- GnuCOBOL Programmer's Guide の Texinfo ソースと、GnuCOBOL の NEWS（SourceForge の SVN）。
- IBM の APAR のページ。

ISO/IEC 1989（COBOL の規格）の本文は有料で、読んでいない。
規格の規定として書くことは、規格を引いている処理系の文書を根拠にし、そうでないものは「未確認事項」に置いた。

## 再実行の方法

```sh
docs/research/cobol-decimal-semantics/run.sh              # expected/ と照合する
docs/research/cobol-decimal-semantics/run.sh --record     # expected/ を書き直す
docs/research/cobol-decimal-semantics/check-sources.sh    # 引用が文書にあるかを確かめる
```

`run.sh` は `flake.lock` と同じリビジョンの nixpkgs から GnuCOBOL 3.1.2 を取得し、`probes/` の各プログラムをコンパイルして実行する。
記録には、コンパイラの判定と警告、プログラムの出力、終了状態が入る。
このマシン（x86_64 Linux）では9件とも一致した。
`github:` の flake 参照を取得できない環境では、`ZUSTRAL_NIXPKGS=path:<nixpkgs のチェックアウト>` で差し替える。

`check-sources.sh` はネットワークに出て、IBM の PDF（2冊で約10MB）も取得する。
`pdftotext` が要る。
このマシンでは、30件の引用が全件見つかった。

## `PICTURE` と桁の上限

`PIC S9(7)V99` は、整数部7桁と小数部2桁の符号付き10進数を表す。
`S` は符号があることを、`V` は小数点の位置を示し、どちらも記憶域の桁を占めない。
スケール（小数部の桁数）と精度（全体の桁数）が、データ項目の宣言に入っている。

桁数の上限は処理系と設定で違う。

- IBM では、2進と10進の項目は既定で1〜18桁で、コンパイラオプション `ARITH(EXTEND)` で31桁まで広がる 〔src: ibm-lr-digits〕 〔src: ibm-pg-arith-extend〕。
- IBM LR は、COBOL 85 の規格がリテラルを18桁までとしていると注記している 〔src: ibm-lr-85-literal〕。
- GnuCOBOL 3.1.2 は `PIC 9(38)` を受け付け、`PIC 9(39)` を拒んだ 〔probe: digits-38 / compile: ok〕 〔probe: digits-39 / line 6: error: numeric field cannot be larger than 38 digits〕。

## 中間結果の精度

IBM は固定小数点の中間結果を、`ARITH(COMPAT)` で最大30桁、`ARITH(EXTEND)` で最大31桁で持つ 〔src: ibm-pg-compat-30〕 〔src: ibm-pg-extend-31〕。
中間結果がこの桁を超えると、コンパイラは30桁か31桁に切り詰める 〔src: ibm-pg-truncate-intermediate〕。
size error は最終結果にだけ適用され、中間結果の切り詰めでは起きない 〔src: ibm-lr-size-final-only〕。
Micro Focus は、`ARITHMETIC"MF"` の設定で中間結果を40桁の内部浮動小数点レジスタで評価すると書いている 〔src: mf-40-digits〕。
GnuCOBOL は中間結果を GMP（任意精度の整数ライブラリ）で持つ 〔推論: コンパイラが生成した C が `gmp.h` を要求したことから〕。
36桁の 999…9 を2乗して同じ数で割ると、72桁の途中結果を経て元の値に戻った 〔probe: intermediate / 36 nines squared then divided=999999999999999999999999999999999999〕。

同じ式でも、中間結果の桁の決め方は処理系とオプションで違う。
IBM の 30桁と31桁、Micro Focus の40桁の浮動小数点、GnuCOBOL の任意精度である。

除算を含む式の結果は、演算の順序でも変わった。
受け取り項目は `PIC 9V9(6)` である。

- `1 / 3 * 3` は 0.999999 になった 〔probe: intermediate / 1 / 3 * 3=0.999999〕。
- `1 * 3 / 3` は 1.000000 になった 〔probe: intermediate / 1 * 3 / 3=1.000000〕。
- `1 / 3 + 1 / 3 + 1 / 3` は 0.999999 になった 〔probe: intermediate / 1 / 3 + 1 / 3 + 1 / 3=0.999999〕。

COBOL 2014 の `ARITHMETIC IS STANDARD-DECIMAL` は、処理系をまたいで中間結果をそろえるための指定である 〔推論: 名前と、GnuCOBOL が他の指定と並べて未実装と扱っていることから〕。
GnuCOBOL 3.1.2 は構文としては受け付けたが、実装されていないと警告した 〔probe: arithmetic-standard-decimal / line 5: warning: STANDARD-DECIMAL ARITHMETIC is not implemented [-Wpending]〕。
IBM LR 6.4 の本文には `STANDARD-DECIMAL` も `ROUNDED MODE` も出てこない（`pdftotext` で抜き出した本文の検索で0件だった）。

## 丸め

IBM では、`ROUNDED` を書かずに小数部があふれると切り捨てになる 〔src: ibm-lr-truncate-unless-rounded〕。
`ROUNDED` を書くと、あふれた部分の最上位の桁が5以上のときに1を足す 〔src: ibm-lr-rounded-half-up〕。
これは四捨五入（`NEAREST-AWAY-FROM-ZERO`）である。

GnuCOBOL の文書も同じ既定を書いている。
`ROUNDED` がなければ `ROUNDED MODE IS TRUNCATION` と同じで、`MODE` のない `ROUNDED` は `ROUNDED MODE IS NEAREST-AWAY-FROM-ZERO` と同じである 〔src: gnucobol-guide-no-rounded〕 〔src: gnucobol-guide-rounded〕。

GnuCOBOL 3.1.2 は COBOL 2014 の `ROUNDED MODE IS <モード>` を受け付けた 〔probe: rounding / compile: ok〕。
同じ値を、小数2桁の受け取り項目 `PIC S9V99` へ各モードで入れた結果は次のとおりである 〔probe: rounding〕。

| モード | 2.345 | 2.355 | -2.345 | 2.3451 | 2.344 | 2.349 |
|---|---|---|---|---|---|---|
| `ROUNDED` なし | +2.34 | +2.35 | -2.34 | +2.34 | +2.34 | +2.34 |
| `ROUNDED` だけ | +2.35 | +2.36 | -2.35 | +2.35 | +2.34 | +2.35 |
| `AWAY-FROM-ZERO` | +2.35 | +2.36 | -2.35 | +2.35 | +2.35 | +2.35 |
| `NEAREST-AWAY-FROM-ZERO` | +2.35 | +2.36 | -2.35 | +2.35 | +2.34 | +2.35 |
| `NEAREST-EVEN` | +2.34 | +2.36 | -2.34 | +2.35 | +2.34 | +2.35 |
| `NEAREST-TOWARD-ZERO` | +2.34 | +2.35 | -2.34 | +2.35 | +2.34 | +2.35 |
| `TOWARD-GREATER` | +2.35 | +2.36 | -2.34 | +2.35 | +2.35 | +2.35 |
| `TOWARD-LESSER` | +2.34 | +2.35 | -2.35 | +2.34 | +2.34 | +2.34 |
| `TRUNCATION` | +2.34 | +2.35 | -2.34 | +2.34 | +2.34 | +2.34 |

`ROUNDED` なしの行は、どの列も `TRUNCATION` と同じ値になった。
`ROUNDED` だけの行は、どの列も `NEAREST-AWAY-FROM-ZERO` と同じ値になった。
COBOL の既定は「書かなければ切り捨て、書けば四捨五入」であり、銀行丸め（`NEAREST-EVEN`）は明示しないと使えない。

`OPTIONS` 段落の `DEFAULT ROUNDED MODE IS NEAREST-EVEN` で、プログラム全体の `ROUNDED` の意味を変えられる。
このとき `COMPUTE X ROUNDED = 2.345` は 2.34 になった 〔probe: options-paragraph / rounded 2.345 under default nearest-even=+2.34〕。
同じ `ROUNDED` という語が、プログラムの冒頭の宣言しだいで別の丸めになる。

`ROUNDED MODE IS PROHIBITED` は、丸めが要る値に対して size error を起こした 〔probe: rounding / PROHIBITED 2.345=size-error, X=+2.34〕。
丸めが要らない 2.34 では起こさなかった 〔probe: rounding / PROHIBITED 2.34=+2.34〕。
ただし GnuCOBOL 3.1.2 では、size error の後も受け取り項目に切り捨てた 2.34 が入っていた。
直前の値は 0 である。
GnuCOBOL は 3.2 でこれを変え、丸めが要るときは受け取り項目を変えないようにした 〔src: gnucobol-news-prohibited〕。
同じモード名でも、処理系の版で受け取り項目の値が変わった。

## 桁あふれと `ON SIZE ERROR`

IBM の規則では、size error は次の2つのときに起きる。
1つは、小数点をそろえた結果の絶対値が受け取り項目に入る最大値を超えたときで 〔src: ibm-lr-size-error-when〕、もう1つはゼロ除算である 〔src: ibm-lr-divide-by-zero〕。
`ON SIZE ERROR` を書かなければ切り捨ての規則で値が入り 〔src: ibm-lr-no-phrase〕、書けば受け取り項目は変わらない 〔src: ibm-lr-phrase-unchanged〕。
丸めは size error の検査より先に行う 〔src: ibm-lr-round-before-size〕。
GnuCOBOL の文書は、桁あふれは黙って起きると書いている 〔src: gnucobol-guide-silent〕。

GnuCOBOL 3.1.2 で、受け取り項目 `PIC 9(3)` を使って確かめた結果は次のとおりである。

- 999 に `ADD 1 TO X` を、句なしで実行すると、上位の桁が落ちて 000 になった 〔probe: size-error / add 1 to 999 without phrase=000〕。
- 同じ文に `ON SIZE ERROR` を書くと、処理が走り、X は 999 のまま残った 〔probe: size-error / add 1 to 999 with phrase=size-error, X=999〕。
- 500 に `COMPUTE X = X * 3` を、句なしで実行すると、X は 500 のまま残った 〔probe: size-error / 500 * 3 without phrase=500〕。

句を書かない場合、`ADD` は切り捨て、`COMPUTE` は据え置きになった。
IBM の文書が書く「切り捨ての規則で値が入る」とも、`COMPUTE` の結果は合わない。
句を書かない場合の受け取り項目の値を、プログラムは処理系をまたいで当てにできない 〔推論〕。

ゼロ除算に `ON SIZE ERROR` を書くと、処理が走り、受け取り項目は元の 7 のまま残った 〔probe: size-error / 7 / 0 with phrase=size-error, X=007〕。

下位の桁の切り捨ては size error にならない。
999.96 を `PIC 9(3)V9` に入れると、句を書いていても 999.9 になり、`NOT ON SIZE ERROR` の側が走った 〔probe: size-error / 999.96 truncated into 9(3)V9=999.9〕。
同じ値に `ROUNDED` を付けると 1000.0 に丸め上がって上位にあふれ、size error になった 〔probe: size-error / 999.96 rounded into 9(3)V9=size-error, Y=000.0〕。
「丸めてから桁あふれを検査する」という IBM の規則と同じ順序である。

## `MOVE` と `GIVING` の暗黙の切り捨て

`MOVE` は、小数点の位置をそろえ、受け取り項目に合わせて桁を切り捨てるかゼロで埋める 〔src: ibm-lr-move-align〕。
`ON SIZE ERROR` を書ける文は `ADD`、`COMPUTE`、`DIVIDE`、`MULTIPLY`、`SUBTRACT` で、`MOVE` には書けない 〔src: gnucobol-guide-size-error-verbs〕。

- 12345.678 を `PIC 9(3)V99` へ `MOVE` すると 345.67 になった。上位の 12 と下位の 8 が消えた 〔probe: truncation / move 12345.678 to 9(3)V99=345.67〕。
- `COMPUTE` で同じ値を入れても 345.67 になった 〔probe: truncation / compute 12345.678 to 9(3)V99=345.67〕。
- 1.999 を `PIC 9V9` へ `MOVE` すると 1.9 になった。丸めではなく切り捨てである 〔probe: truncation / move 1.999 to 9V9=1.9〕。

`GIVING` で複数の受け取り項目を書くと、結果をいったん一時領域に置き、そこから各項目へ移す 〔src: ibm-lr-giving-temporary〕。
各項目には、その項目の `PICTURE` に合わせた値が入る。
123.45 + 678.91 = 802.36 を3つの項目へ入れた結果は次のとおりである。

- `PIC 9(2)` には 02 が入った 〔probe: giving / 123.45 + 678.91 giving 9(2)=02〕。
- `PIC 9(4)V9` には 0802.3 が入った 〔probe: giving / 123.45 + 678.91 giving 9(4)V9=0802.3〕。
- `PIC 9(4)V99` には 0802.36 が入った 〔probe: giving / 123.45 + 678.91 giving 9(4)V99=0802.36〕。

`ROUNDED` は受け取り項目ごとに付ける。
`PIC 9(4)V9` に付けると 0802.4 になった 〔probe: giving / rounded 123.45 + 678.91 giving 9(4)V9=0802.4〕。
`ON SIZE ERROR` を付けると、あふれた `PIC 9(2)` だけが元の 00 のまま残り、あふれなかった `PIC 9(4)V9` には 0802.3 が入った 〔probe: giving / size-error with two receivers, R2=00 R5=0802.3〕。
1つの文が、受け取り項目ごとに別々の成否を持つ。

## 符号の消失

IBM の規則では、符号のない項目へ `MOVE` すると、送り側の絶対値が使われる 〔src: ibm-lr-unsigned-move〕。

- 符号のない `PIC 9(3)` へ -5 を `MOVE` すると、005 になった 〔probe: truncation / move -5 to 9(3)=005〕。
- `COMPUTE` で 2 - 7 を入れても 005 になった 〔probe: truncation / compute 2 - 7 to 9(3)=005〕。
- 符号付きの `PIC S9(3)` なら -005 になった 〔probe: truncation / compute 2 - 7 to S9(3)=-005〕。

負の値が符号のない項目に入っても、size error にはならない。

## 実際に起きた不具合

以下はすべて IBM Enterprise COBOL の APAR で、IBM が自社のコンパイラの不具合として認めたものである。
公開の出典で原因が COBOL の10進の意味論だと確かめられる現場の事故は、見つからなかった。

- **桁あふれの検査が飛ばされた。** 最適化が18桁の2進項目どうしの加算を2進の加算で行い、19桁の結果を検査できず、`ON SIZE ERROR` が実行されないことがあった 〔src: apar-ph48674〕。
- **`TRUNC(OPT)` で結果が変わった。** コンパイラが「2進の項目は `PICTURE` の桁に収まる」と仮定して最適化し、旧版と違う結果になった 〔src: apar-pi85244〕。
- **丸めの位置が版で変わった。** COBOL 5/6 の実行時ライブラリが中間結果を小数33桁で丸め、COBOL 4.2 と違う結果を出した。正しくは31桁である 〔src: apar-ph64935〕。
- **符号が版で変わった。** COBOL 4.2 では負になる2進項目が、5.2 の最適化の下では正になった 〔src: apar-pi67723〕。
- **負のゼロが生じた。** 計算結果のゼロが負の符号を持ったまま符号付き項目に入ることがあり、修正でゼロを正の符号にそろえた 〔src: apar-ph44352〕。
- **上位の桁が落ちた。** 10桁以上の相対レコードキーを、実行時ライブラリが下位9桁だけで扱い、大きなファイルで別のレコードを読んだ 〔src: apar-ph30657〕。

どれも、桁数、丸めの位置、符号を「処理系とオプションが暗黙に決める」ところで起きている。
同じソースの結果が、コンパイラの版、最適化の水準、`TRUNC` や `ARITH` のオプションで変わった 〔推論〕。

## 0005 の前提: 16バイトに収まるか

[0005: Decimal は線形リソースか、値型か](https://github.com/seiei-sogen/zustral/issues/5) は、必要な桁数が16バイトの固定幅に収まることを前提にしている。
16バイトの候補の桁数は、decimal128 で34桁、`__int128` で38桁である（2の127乗は約 1.7 × 10の38乗なので、38桁までの整数はすべて入る）。

- IBM の項目の上限31桁と中間結果の上限31桁は、decimal128 にも `__int128` にも収まる 〔src: ibm-pg-arith-extend〕 〔src: ibm-pg-extend-31〕。
- GnuCOBOL の上限38桁は、`__int128` には収まるが、decimal128 の34桁は超える 〔probe: digits-38 / compile: ok〕。
- IBM の既定の18桁どうしを掛けると、積は最大36桁になり、decimal128 の34桁を超える。IBM はこれを中間結果の31桁への切り詰めで扱っている 〔src: ibm-pg-truncate-intermediate〕 〔推論〕。

項目の桁数については、0005 の前提は IBM の範囲では成り立つ。
ただし、中間結果を16バイトに収めると、乗算の途中で桁が足りなくなる場合がある。
そのときにどうするかは、[0008: 桁あふれを例外なしでどう扱うか](https://github.com/seiei-sogen/zustral/issues/8) の材料になる。

## 継ぐ候補と避ける候補

以下は上の事実から導いた候補であり、決定ではない 〔推論〕。

継ぐ候補は次のとおりである。

- 精度とスケールをデータの宣言に書くこと（`PIC S9(7)V99`）。
- 8つの丸めモードの名前。そのうち `NEAREST-EVEN`、`NEAREST-AWAY-FROM-ZERO`、`TOWARD-GREATER`、`TOWARD-LESSER`、`TRUNCATION` の5つは、IEEE 754 の5つの丸め方向に1対1で対応する。
- 丸めてから桁あふれを検査する順序。
- 丸めが要るときに失敗させる `PROHIBITED` という選択肢。

避ける候補は次のとおりである。

- 上位の桁を黙って落とすこと。`MOVE` と、句を書かない算術文の両方で起きる。
- 丸めを書かないと切り捨てになる既定。丸めの有無と種類を毎回書かせれば、既定がそもそも要らない。
- `DEFAULT ROUNDED MODE` のように、離れた場所の宣言で `ROUNDED` の意味が変わること。暗黙の変換を禁じる Austral の方針と合わない。
- 中間結果の桁数を処理系とオプションに任せること。APAR の不具合の多くがここで起きている。
- 符号のない項目へ入れると符号が消えること。
- 句の有無で、桁あふれ後の受け取り項目の値が変わること。

## 下流の問いに効く事実

### 0004「スケールは型に入るか、値に入るか」

COBOL はスケールを型（データ項目の宣言）に入れている。
その代わり、宣言より大きい値は `MOVE` でも算術文でも黙って切り捨てる。
スケールを型に入れるなら、型の違う値の間の代入で、切り捨てを明示させるかどうかが同時に問われる。

### 0006「除算のスケールを誰が決めるか」

COBOL では、結果のスケールは受け取り項目の宣言で決まる。
除算の中間結果のスケールは処理系が決める。IBM は、最終結果の項目に要る小数桁数を、中間結果に持たせる小数桁数の候補に入れている 〔src: ibm-pg-dmax〕。
`1 / 3 * 3` と `1 * 3 / 3` が違う結果になる 〔probe: intermediate〕。

### 0007「丸めをどう明示するか」

8つのモード名と、`PROHIBITED` という「丸めが要るなら失敗」という選択肢がある。
既定を持つと、`ROUNDED` だけで四捨五入、なしで切り捨てという暗黙の振る舞いが生じる。

### 0008「桁あふれを例外なしでどう扱うか」

COBOL の `ON SIZE ERROR` は、例外ではなく、文に付けた分岐である。
句を書くと受け取り項目は変わらず、書かないと処理系しだいの値が入る。
複数の受け取り項目では、項目ごとに成否が分かれる。

### 0009「Decimal の算術を演算子で書けるようにするか」

COBOL の `COMPUTE` は式を書けるが、丸めと桁あふれの指定は式の演算子ではなく、文（受け取り項目）に付く。
演算子ごとには丸めと桁あふれを指定できない。

## 未確認事項

- ISO/IEC 1989:2002 と 2014 の本文は読んでいない。規格での数値項目の桁の上限、8つの丸めモードの一覧が規格の一覧と同じかどうか、`ARITHMETIC IS STANDARD-DECIMAL` が decimal128 の中間結果を使うかどうかは、確かめていない。
- 規格が、`ON SIZE ERROR` を書かない場合の値を「未定義」としているかどうかは確かめていない。IBM は「切り捨ての規則で値が入る」と書き、GnuCOBOL 3.1.2 の `COMPUTE` は値を変えなかった。
- 中間結果の規則は IBM のものだけを読んだ。Micro Focus は40桁の浮動小数点という1文だけを確かめた。
- バンクーバー証券取引所の指数（1982年）のような、よく知られた切り捨ての事故は、出典を取得できず、COBOL との関係も確かめられないので本文から外した。
- GnuCOBOL の観測は 3.1.2 のものである。3.2 では少なくとも `PROHIBITED` の振る舞いが変わっている 〔src: gnucobol-news-prohibited〕。

## ファイル

- `run.sh` は、GnuCOBOL で `probes/` を再実行し、`expected/` と照合する。
- `probes/*.cob` は、確かめたい規則ごとの COBOL プログラムである。自由形式（`cobc -free`）で書いた。
- `expected/*.txt` は、各プログラムの記録である。
- `check-sources.sh` は、`sources.tsv` の各 URL を取得し、引用が文書にあるかを確かめる。
- `sources.tsv` は、ID、URL、引用の3列の表である。
