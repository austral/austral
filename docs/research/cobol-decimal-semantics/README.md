# COBOL の10進意味論から何を継ぐか（issue #3）

COBOL が10進計算について定めている規則を、Zustral の Decimal の設計材料として並べたレポートである。
対象は、`PICTURE` による桁とスケールの宣言、中間結果の精度、`ROUNDED` と丸めモード、`ON SIZE ERROR`、`GIVING` と `MOVE` の暗黙の切り捨て、符号の消失である。
あわせて、これらの規則が実際に起こした不具合を IBM の APAR（コンパイラの不具合報告）から拾った。

このレポートは何を継ぐかを決めない。
決めるのは issue #4（0004）、#6（0006）、#7（0007）、#8（0008）の仕事である。
ここでは、各問いに効く事実を名指しするところまでにとどめる。

## 読み方

事実を述べる文には、次のどれかの根拠を付ける。

- `〔probe: <記録> / <行>〕` は、`expected/<記録>.txt` の行を指す。`run.sh` が GnuCOBOL 3.1.2 で再実行して照合する。GnuCOBOL という1つの処理系での観測であり、規格の規定ではない。
- `〔src: <ID>〕` は、`sources.tsv` の行を指す。行には URL と、ページから写した引用がある。`check-sources.sh` が、引用がいまもページにあるかを確かめる。
- `〔推論〕` は、上の根拠から導いたが、直接は確かめていない主張に付ける。

ISO/IEC 1989（COBOL の規格）の本文は有料で、読んでいない。
規格の規定として書くことは、規格を引いている処理系の文書を根拠にした。

## 再実行の方法

```sh
docs/research/cobol-decimal-semantics/run.sh              # expected/ と照合する
docs/research/cobol-decimal-semantics/run.sh --record     # expected/ を書き直す
docs/research/cobol-decimal-semantics/check-sources.sh    # 引用がページにあるかを確かめる
```

`run.sh` は `flake.lock` と同じリビジョンの nixpkgs から GnuCOBOL 3.1.2 を取得し、`probes/` の各プログラムをコンパイルして実行する。
記録には、コンパイラの判定と警告、プログラムの出力、終了状態が入る。
このマシン（x86_64 Linux）では9件とも一致した。
`github:` の flake 参照を取得できない環境では、`ZUSTRAL_NIXPKGS=path:<nixpkgs のチェックアウト>` で差し替える。

`check-sources.sh` はネットワークに出る。
このマシンでは、引用は全件ページ上で見つかった。

## 丸めモード

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

`ROUNDED` を書かないと切り捨てになり、どの列も `TRUNCATION` と同じ値になった。
`ROUNDED` だけを書くと、どの列も `NEAREST-AWAY-FROM-ZERO`（四捨五入）と同じ値になった。
つまり COBOL の既定は「書かなければ切り捨て、書けば四捨五入」であり、銀行丸め（`NEAREST-EVEN`）は明示しないと使えない。

`OPTIONS` 段落の `DEFAULT ROUNDED MODE IS NEAREST-EVEN` で、プログラム全体の `ROUNDED` の意味を変えられる。
このとき `COMPUTE X ROUNDED = 2.345` は 2.34 になった 〔probe: options-paragraph / rounded 2.345 under default nearest-even=+2.34〕。
同じ `ROUNDED` という語が、プログラムの冒頭の宣言しだいで別の丸めになる。

`ROUNDED MODE IS PROHIBITED` は、丸めが要る値に対して size error を起こした 〔probe: rounding / PROHIBITED 2.345=size-error, X=+2.34〕。
丸めが要らない 2.34 では起こさなかった 〔probe: rounding / PROHIBITED 2.34=+2.34〕。
ただし size error を起こした後も、受け取り項目には切り捨てた 2.34 が入っていた。
直前の値は 0 である。
後の節で見るとおり、`ADD` や `COMPUTE` の size error では、`ON SIZE ERROR` を書くと受け取り項目は元の値のまま残る。
`PROHIBITED` だけがこれと違う動きをした 〔probe: rounding〕 〔probe: size-error〕。

## 桁あふれと `ON SIZE ERROR`

受け取り項目 `PIC 9(3)` に 999 を入れて1を足した結果は、書き方で変わった。

- `ON SIZE ERROR` を書かない `ADD 1 TO X` では、上位の桁が落ちて 000 になった 〔probe: size-error / add 1 to 999 without phrase=000〕。
- `ON SIZE ERROR` を書くと、処理が走り、X は 999 のまま残った 〔probe: size-error / add 1 to 999 with phrase=size-error, X=999〕。
- `ON SIZE ERROR` を書かない `COMPUTE X = X * 3`（X は 500）では、X は 500 のまま残った 〔probe: size-error / 500 * 3 without phrase=500〕。

同じ処理系の同じ既定の設定で、句を書かないときの結果が `ADD` では切り捨て、`COMPUTE` では据え置きと分かれた。
句を書かない場合の受け取り項目の値を、プログラムは当てにできない。

ゼロ除算は size error として扱われ、`ON SIZE ERROR` の処理が走り、受け取り項目は元の 7 のまま残った 〔probe: size-error / 7 / 0 with phrase=size-error, X=007〕。

下位の桁の切り捨ては size error にならない。
999.96 を `PIC 9(3)V9` に入れると、句を書いていても 999.9 になり、`NOT ON SIZE ERROR` の側が走った 〔probe: size-error / 999.96 truncated into 9(3)V9=999.9〕。
同じ値に `ROUNDED` を付けると 1000.0 に丸め上がって上位にあふれ、size error になった 〔probe: size-error / 999.96 rounded into 9(3)V9=size-error, Y=000.0〕。
丸めが桁あふれを起こしうるので、丸めと桁あふれの検査は順序の決まった一続きの処理として扱われている 〔推論〕。

## `MOVE` と `GIVING` の暗黙の切り捨て

`MOVE` は、受け取り項目の `PICTURE` に合わせて上位と下位の桁を黙って落とした。

- 12345.678 を `PIC 9(3)V99` へ `MOVE` すると 345.67 になった。上位の 12 と下位の 8 が消えた 〔probe: truncation / move 12345.678 to 9(3)V99=345.67〕。
- `COMPUTE` で同じ値を入れても 345.67 になった 〔probe: truncation / compute 12345.678 to 9(3)V99=345.67〕。
- 1.999 を `PIC 9V9` へ `MOVE` すると 1.9 になった。丸めではなく切り捨てである 〔probe: truncation / move 1.999 to 9V9=1.9〕。

`GIVING` は、1つの結果を複数の受け取り項目へ、それぞれの `PICTURE` で格納した。
123.45 + 678.91 = 802.36 を3つの項目へ入れた結果は次のとおりである。

- `PIC 9(2)` には 02 が入った 〔probe: giving / 123.45 + 678.91 giving 9(2)=02〕。
- `PIC 9(4)V9` には 0802.3 が入った 〔probe: giving / 123.45 + 678.91 giving 9(4)V9=0802.3〕。
- `PIC 9(4)V99` には 0802.36 が入った 〔probe: giving / 123.45 + 678.91 giving 9(4)V99=0802.36〕。

`ROUNDED` は受け取り項目ごとに付ける。
`PIC 9(4)V9` に付けると 0802.4 になった 〔probe: giving / rounded 123.45 + 678.91 giving 9(4)V9=0802.4〕。

`ON SIZE ERROR` を付けると、あふれた `PIC 9(2)` だけが元の 00 のまま残り、あふれなかった `PIC 9(4)V9` には 0802.3 が入った 〔probe: giving / size-error with two receivers, R2=00 R5=0802.3〕。
1つの文が、受け取り項目ごとに別々の成否を持つ。

## 符号の消失

符号のない項目 `PIC 9(3)` へ -5 を `MOVE` すると、005 になった 〔probe: truncation / move -5 to 9(3)=005〕。
`COMPUTE` で 2 - 7 を入れても 005 になった 〔probe: truncation / compute 2 - 7 to 9(3)=005〕。
符号付きの `PIC S9(3)` なら -005 になる 〔probe: truncation / compute 2 - 7 to S9(3)=-005〕。
負の値が符号のない項目に入っても、size error にはならない。

## 中間結果の精度

除算を含む式の結果は、演算の順序で変わった。
受け取り項目は `PIC 9V9(6)` である。

- `1 / 3 * 3` は 0.999999 になった 〔probe: intermediate / 1 / 3 * 3=0.999999〕。
- `1 * 3 / 3` は 1.000000 になった 〔probe: intermediate / 1 * 3 / 3=1.000000〕。
- `1 / 3 + 1 / 3 + 1 / 3` は 0.999999 になった 〔probe: intermediate / 1 / 3 + 1 / 3 + 1 / 3=0.999999〕。

除算の中間結果は、有限の桁で打ち切られている。
何桁で打ち切るかは処理系が決める。

36桁の 999…9 を2乗して同じ数で割ると、GnuCOBOL では元の値に戻った 〔probe: intermediate / 36 nines squared then divided=999999999999999999999999999999999999〕。
2乗の途中は72桁になる。
GnuCOBOL は中間結果を GMP（任意精度の整数ライブラリ）で持つので、桁を失わない 〔推論: コンパイラが `gmp.h` を要求したことから〕。

## 桁の上限

GnuCOBOL 3.1.2 は `PIC 9(38)` を受け付け、`PIC 9(39)` を拒んだ 〔probe: digits-38 / compile: ok〕 〔probe: digits-39 / line 6: error: numeric field cannot be larger than 38 digits〕。

COBOL 2014 の `ARITHMETIC IS STANDARD-DECIMAL` は、構文としては通ったが、実装されていないという警告が出た 〔probe: arithmetic-standard-decimal / line 5: warning: STANDARD-DECIMAL ARITHMETIC is not implemented [-Wpending]〕。
