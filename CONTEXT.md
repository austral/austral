# Zustral Decimal

Zustral（Austral のフォーク）に金融向けの10進計算を持ち込むための語彙である。
決定の経緯は GitHub の地図 issue #9 とその子チケットにある。

## Language

**Decimal**:
Zustral の組み込みの10進固定小数点型。精度とスケールを型引数に取り、`Decimal[P, S]` と書く。
_Avoid_: 10進浮動小数点、BigDecimal、Money

**精度**:
Decimal が持てる全体の桁数。型引数 P で、1 以上 38 以下の整数リテラル。
_Avoid_: 桁数、長さ、幅

**スケール**:
Decimal の小数点より下の桁数。型引数 S で、0 以上 P 以下の整数リテラル。
_Avoid_: 小数桁、指数

**係数**:
Decimal の値を 10 の S 乗倍した整数。値は「係数 × 10 の −S 乗」である。
_Avoid_: 仮数

**丸めモード**:
桁を落とすときに、どちら向きに丸めるかを表す値。組み込みの union 型 `RoundingMode` の7つの case のどれか。既定の丸めモードはない。
_Avoid_: 丸め方式、端数処理

**桁あふれ**:
演算の結果の係数の絶対値が、結果の型の精度 P で表せる範囲を超えること。
_Avoid_: オーバーフロー、size error
