# QOI

This is an image compression library based on the QOI format.
https://qoiformat.org/

<h3>Types</h3>

```
public type Description = {
  width : Nat32;
  height : Nat32;
  channels: Nat8;
  colorspace: Nat8;
};
```
```
public type SharedBitmap = {
  desc : Description;
  data : [Nat8];
};
```
```
public type SharedQOI = {
  desc : Description;
  data : [Nat8];
};
```
```
public type Bitmap = {
  desc : Description;
  data: Buffer.Buffer<Nat8>;
};
```
```
public type QOI = {
  desc : Description;
  data: Buffer.Buffer<Nat8>;
};
```

<h3>Functions</h3>

```
public func toSharedQOI(input : QOI) : SharedQOI
```
```
public func toSharedBitmap(input : Bitmap) : SharedBitmap
```
```
public func fromSharedQOI(input : SharedQOI) : QOI
```
```
public func fromSharedBitmap(input : SharedBitmap) : Bitmap
```
```
public func encode(input : Bitmap) : Result.Result<QOI, ()>
```
```
public func decode(input : QOI) : Result.Result<Bitmap, ()>
```