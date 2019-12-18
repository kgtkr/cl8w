# cl8w

[![Build Status](https://travis-ci.org/kgtkr/cl8w.svg?branch=master)](https://travis-ci.org/kgtkr/cl8w)

WebAssemblyで動く実験的な言語

## 特徴
* LLVMなどに一切依存せず直接wasmバイナリを吐きます

## 使い方
まだcliなどは完成していません。ビルドして実行するとtest.cl8wがコンパイルされtest.wasmが出力されます。
`node test.js`でtest.wasmを読み込み実行出来ます。

## 注意
エラー処理などは全くしていないので少しでも間違ったコードを書くとコンパイラがクラッシュするか不正なwasmを吐くか未定義動作を踏みます。
またまだバグだらけです。

## 構文
### 関数定義
```
fun ident(ident: type, *): type? = expr
```

### 外部関数
```
extern fun "module" "name" ident(ident: type, *): type?
```

### 構造体
```
struct Ident {
    ident: type, ...
}
```

### 式
今度書く