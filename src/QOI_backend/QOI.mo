import Array "mo:base/Array";
import Buffer "mo:base/Buffer";
import Int8 "mo:base/Int8";
import Int32 "mo:base/Int32";
import Nat8 "mo:base/Nat8";
import Nat32 "mo:base/Nat32";
import Prim "mo:⛔";
import Result "mo:base/Result";
import Iter "mo:base/Iter";

module {
  public type Description = {
    width : Nat32;
    height : Nat32;
    channels: Nat8;
    colorspace: Nat8;
  };

  public type SharedBitmap = {
    desc : Description;
    data : [Nat8];
  };

  public type SharedQOI = {
    desc : Description;
    data : [Nat8];
  };

  public type Bitmap = {
    desc : Description;
    data: Buffer.Buffer<Nat8>;
  };

  public type QOI = {
    desc : Description;
    data: Buffer.Buffer<Nat8>;
  };

  public type PerceptualHash = {
    theHash : [Nat8];
  };

  let QOI_PIXELS_MAX : Nat32 = 400000000;
  let QOI_OP_INDEX : Nat8 = 0x00;
  let QOI_OP_DIFF : Nat8 = 0x40;
  let QOI_OP_LUMA : Nat8 = 0x80;
  let QOI_OP_RUN : Nat8 = 0xc0;
  let QOI_OP_RGB : Nat8 = 0xfe;
  let QOI_OP_RGBA : Nat8 = 0xff;
  let QOI_MASK_2 : Nat8 = 0xc0;

  private type QOI_RGBA = {
    r: Nat8;
    g: Nat8;
    b: Nat8;
    a: Nat8;
  };

  private func hash(pixel : QOI_RGBA) : Nat {
    return 3 * Nat8.toNat(pixel.r) + 5 * Nat8.toNat(pixel.g) + 7 * Nat8.toNat(pixel.b) + 11 * Nat8.toNat(pixel.a);
  };

  private func computeV(pixel : QOI_RGBA) : Nat32 {
    var r : Nat32 = Prim.natToNat32(Nat8.toNat(pixel.r));
    var g : Nat32 = Prim.natToNat32(Nat8.toNat(pixel.g));
    var b : Nat32 = Prim.natToNat32(Nat8.toNat(pixel.b));
    var a : Nat32 = Prim.natToNat32(Nat8.toNat(pixel.a));
    r := Nat32.bitshiftLeft(r, 24);
    g := Nat32.bitshiftLeft(g, 16);
    b := Nat32.bitshiftLeft(b, 8);
    return r | g | b | a;
  };

  private func subtractNat8(a : Nat8, b : Nat8) : Int8 {
    return Int8.fromIntWrap(Prim.int32ToInt(Prim.nat32ToInt32(Prim.natToNat32(Prim.nat8ToNat(a))) 
            - Prim.nat32ToInt32(Prim.natToNat32(Prim.nat8ToNat(b)))));
  };

  public func subtractInt8(a : Int8, b : Int8) : Int8 {
    return Int8.fromIntWrap((Int8.toInt(a) - Int8.toInt(b)));
  };

  public func toSharedQOI(input : QOI) : SharedQOI {
    return {
      desc = input.desc;
      data = input.data.toArray();
    }
  };

  public func toSharedBitmap(input : Bitmap) : SharedBitmap {
    return {
      desc = input.desc;
      data = input.data.toArray();
    }
  };

  public func fromSharedQOI(input : SharedQOI) : QOI {
    let data0 : Buffer.Buffer<Nat8> = Buffer.Buffer<Nat8>(0);
    for (x in input.data.vals()) {
      data0.add(x);
    };
    return {
      desc = input.desc;
      data = data0;
    }
  };

  public func fromSharedBitmap(input : SharedBitmap) : Bitmap {
    let data0 : Buffer.Buffer<Nat8> = Buffer.Buffer<Nat8>(0);
    for (x in input.data.vals()) {
      data0.add(x);
    };
    return {
      desc = input.desc;
      data = data0;
    }
  };

  public func encode(input : Bitmap) : Result.Result<QOI, ()> {
    if (input.data.size() == 0 
      or input.desc.width == 0 
      or input.desc.height == 0 
      or input.desc.channels < 3 
      or input.desc.channels > 4 
      or input.desc.colorspace > 1 
      or input.desc.height * input.desc.width * Prim.natToNat32(Nat8.toNat(input.desc.channels)) >= QOI_PIXELS_MAX) {
      return #err();
    };

    let index : [var QOI_RGBA] = Array.init<QOI_RGBA>(64, {
      r = 0;
      g = 0;
      b = 0;
      a = 0;
    });
    var px : QOI_RGBA = {      
      r = 0;
      g = 0;
      b = 0;
      a = 0;
    };
    var pxPrevious : QOI_RGBA = {      
      r = 0;
      g = 0;
      b = 0;
      a = 255;
    };

    let output : QOI = {
      desc = input.desc;
      data = Buffer.Buffer<Nat8>(0);
    };

    var run : Nat32 = 0;

    let pxLength : Nat32 = input.desc.width * input.desc.height * Prim.natToNat32(Prim.nat8ToNat(input.desc.channels));
    let pxEnd : Nat32 = pxLength - Prim.natToNat32(Prim.nat8ToNat(input.desc.channels));
    let channels : Nat32 = Prim.natToNat32(Prim.nat8ToNat(input.desc.channels));

    var pxPos : Nat32 = 0;
    while (pxPos < pxLength) {
      px := {      
        r = input.data.get(Prim.nat32ToNat(pxPos) + 0);
        g = input.data.get(Prim.nat32ToNat(pxPos) + 1);
        b = input.data.get(Prim.nat32ToNat(pxPos) + 2);
        a = px.a;
      };
      if (channels == 4) {
        px := {
          r = px.r;
          g = px.g;
          b = px.b;
          a = input.data.get(Prim.nat32ToNat(pxPos) + 3);
        };
      };
      if (computeV(px) == computeV(pxPrevious)) {
        run := run + 1;
        if (run == 62 or pxPos == pxEnd) {
          output.data.add(QOI_OP_RUN | (Prim.natToNat8(Prim.nat32ToNat(run)) - 1));
          run := 0;
        };
      } else {
        if (run > 0) {
          output.data.add(QOI_OP_RUN | (Prim.natToNat8(Prim.nat32ToNat(run)) - 1));
          run := 0;
        };

        let indexPos = hash(px) % 64;

        if (computeV(index[indexPos]) == computeV(px)) {
          output.data.add(QOI_OP_INDEX | Prim.natToNat8(indexPos));
        } else {
          index[indexPos] := px;

          if (px.a == pxPrevious.a) {
            let vr : Int8 = subtractNat8(px.r, pxPrevious.r);
            let vg : Int8 = subtractNat8(px.g, pxPrevious.g);
            let vb : Int8 = subtractNat8(px.b, pxPrevious.b);

            let vgR : Int8 = subtractInt8(vr, vg);
            let vgB : Int8 = subtractInt8(vb, vg);

            if (vr > -3 and vr < 2 and vg > -3 and vg < 2 and vb > -3 and vb < 2) {
              output.data.add(QOI_OP_DIFF | Prim.int8ToNat8((vr + 2) << 4) | Prim.int8ToNat8((vg + 2) << 2) 
                              | Prim.int8ToNat8((vb + 2)));
            } else if(vgR > -9 and vgR < 8 and vg > -33 and vg < 32 and vgB > -9 and vgB < 8) {
              output.data.add(QOI_OP_LUMA | Prim.int8ToNat8(vg + 32));
              output.data.add(Prim.int8ToNat8(((vgR + 8) << 4) | (vgB + 8)));
            } else {
              output.data.add(QOI_OP_RGB);
              output.data.add(px.r);
              output.data.add(px.g);
              output.data.add(px.b);
            };
          } else {
              output.data.add(QOI_OP_RGBA);
              output.data.add(px.r);
              output.data.add(px.g);
              output.data.add(px.b);
              output.data.add(px.a);
          };
        };
      };
      pxPrevious := px;

      pxPos := pxPos + channels;
    };

    return #ok(output);
  };

  public func decode(input : QOI) : Result.Result<Bitmap, ()> {
    if (input.data.size() == 0 
      or input.desc.width == 0 
      or input.desc.height == 0 
      or input.desc.channels < 3 
      or input.desc.channels > 4 
      or input.desc.colorspace > 1 
      or input.desc.height * input.desc.width * Prim.natToNat32(Nat8.toNat(input.desc.channels)) >= QOI_PIXELS_MAX) {
      return #err();
    };

    let index : [var QOI_RGBA] = Array.init<QOI_RGBA>(64, {
      r = 0;
      g = 0;
      b = 0;
      a = 0;
    });
    var px : QOI_RGBA = {      
      r = 0;
      g = 0;
      b = 0;
      a = 255;
    };

    let output : Bitmap = {
      desc = input.desc;
      data = Buffer.Buffer<Nat8>(0);
    };

    var p : Nat32 = 0;
    var run : Nat32 = 0;

    let pxLength : Nat32 = input.desc.width * input.desc.height * Prim.natToNat32(Prim.nat8ToNat(input.desc.channels));
    let chunksLength : Nat32 = Nat32.fromNat(input.data.size());
    let channels : Nat32 = Prim.natToNat32(Prim.nat8ToNat(input.desc.channels));

    var pxPos : Nat32 = 0;
    while (pxPos < pxLength) {
      if (run > 0) {
        run := run - 1;
      } else if(p < chunksLength) {
        let b1 : Int32 = Prim.nat32ToInt32(Prim.natToNat32(Nat8.toNat(input.data.get(Nat32.toNat(p)))));
        p := p + 1;
        if (Prim.intToNat8Wrap(Int32.toInt(b1)) == QOI_OP_RGB) {
              px := {      
                r = input.data.get(Nat32.toNat(p));
                g = input.data.get(Nat32.toNat(p + 1));
                b = input.data.get(Nat32.toNat(p + 2));
                a = px.a;
              };
              p := p + 3;
        } else if (Prim.intToNat8Wrap(Int32.toInt(b1)) == QOI_OP_RGBA) {
              px := {      
                r = input.data.get(Nat32.toNat(p));
                g = input.data.get(Nat32.toNat(p + 1));
                b = input.data.get(Nat32.toNat(p + 2));
                a = input.data.get(Nat32.toNat(p + 3));
              };
              p := p + 4;
        } else if ((Prim.intToNat8Wrap(Int32.toInt(b1)) & QOI_MASK_2) == QOI_OP_INDEX) {
          px := index[Nat8.toNat(Prim.intToNat8Wrap(Int32.toInt(b1)))];
        } else if ((Prim.intToNat8Wrap(Int32.toInt(b1)) & QOI_MASK_2) == QOI_OP_DIFF) {
              px := {      
                r = Prim.intToNat8Wrap(Int32.toInt(Prim.nat32ToInt32(Prim.natToNat32(Nat8.toNat(px.r))) 
                + ((b1 >> 4) & 0x03) - 2));
                g = Prim.intToNat8Wrap(Int32.toInt(Prim.nat32ToInt32(Prim.natToNat32(Nat8.toNat(px.g))) 
                + ((b1 >> 2) & 0x03) - 2));
                b = Prim.intToNat8Wrap(Int32.toInt(Prim.nat32ToInt32(Prim.natToNat32(Nat8.toNat(px.b))) 
                + (b1 & 0x03) - 2));
                a = px.a;
              };
        } else if ((Prim.intToNat8Wrap(Int32.toInt(b1)) & QOI_MASK_2) == QOI_OP_LUMA) {
          let b2 : Int32 = Prim.nat32ToInt32(Prim.natToNat32(Nat8.toNat(input.data.get(Nat32.toNat(p)))));
          p := p + 1;
          let vg : Int32 = (Prim.nat32ToInt32(Prim.natToNat32(Nat8.toNat(Prim.intToNat8Wrap(Int32.toInt(b1))))) 
          & 0x3f) - 32;
          px := {      
            r = Prim.intToNat8Wrap(Int32.toInt(Prim.nat32ToInt32(Prim.natToNat32(Nat8.toNat(px.r))) + vg - 8 
            + ((b2 >> 4) & 0x0f)));
            g = Prim.intToNat8Wrap(Int32.toInt((Prim.nat32ToInt32(Prim.natToNat32(Nat8.toNat(px.g))) + vg)));
            b = Prim.intToNat8Wrap(Int32.toInt(Prim.nat32ToInt32(Prim.natToNat32(Nat8.toNat(px.b))) + vg - 8 
            + (b2 & 0x0f)));
            a = px.a;
          };
        } else if ((Prim.intToNat8Wrap(Int32.toInt(b1)) & QOI_MASK_2) == QOI_OP_RUN) {
          run := Prim.natToNat32(Nat8.toNat((Prim.intToNat8Wrap(Int32.toInt(b1)) & 0x3f)));
        };
        index[hash(px) % 64] := px;
      };
      output.data.add(px.r);
      output.data.add(px.g);
      output.data.add(px.b);

      if (channels == 4) {
        output.data.add(px.a);
      };

      pxPos := pxPos + channels;
    };

    return #ok(output);
  };

  private func shrink(input : Bitmap) : [Nat8] {
    let output : Buffer.Buffer<Nat8> = Buffer.Buffer<Nat8>(0);

    let xStep : Nat32 = input.desc.width / 16;
    let yStep : Nat32 = input.desc.height / 16;

    var y : Nat32 = 0;
    while (y < 16) {
      var x : Nat32 = 0;
      while (x < 16) {
        output.add(input.data.get(Nat32.toNat(x * xStep + y * yStep * input.desc.width)));
        output.add(input.data.get(Nat32.toNat(x * xStep + y * yStep * input.desc.width) + 1));
        output.add(input.data.get(Nat32.toNat(x * xStep + y * yStep * input.desc.width) + 2));
        x := x + 1;
      };
      y := y + 1;
    };

    return Buffer.toArray(output);
  };

  private func toGrayscale(input : [Nat8]) : [Nat8] {
    let output : Buffer.Buffer<Nat8> = Buffer.Buffer<Nat8>(0);

    var i : Nat32 = 0;
    while (i < 256) {
      output.add(Prim.intToNat8Wrap(Int32.toInt((Prim.nat32ToInt32(Prim.natToNat32(Nat8.toNat(input[Nat32.toNat(i * 3)]))) 
      * 299 + (Prim.nat32ToInt32(Prim.natToNat32(Nat8.toNat(input[Nat32.toNat(i * 3) + 1]))) * 587) 
      + (Prim.nat32ToInt32(Prim.natToNat32(Nat8.toNat(input[Nat32.toNat(i * 3) + 2]))) * 114)) / 1000)));
      i := i + 1;
    };

    return Buffer.toArray(output);
  };

  private func averageValue(input : [Nat8]) : Nat8 {
    var sum : Nat = 0;
    for (i in Iter.range(0, 255)) {
      sum := sum + Nat8.toNat(input[i]);
    };
    return Nat8.fromNat(sum / 256);
  };

  public func computePerceptualHash(input : Bitmap) : Result.Result<PerceptualHash, ()> {
    let shrinked : [Nat8] = shrink(input);
    let grayscale : [Nat8] = toGrayscale(shrinked);

    let average : Nat8 = averageValue(grayscale);
    let hash : Buffer.Buffer<Nat8> = Buffer.Buffer<Nat8>(0);
    for (i in Iter.range(0, 255)) {
      if (grayscale[i] > average) {
        hash.add(1);
      } else {
        hash.add(0);
      };
    };
    return #ok({
      theHash = Buffer.toArray(hash);
    });
  };
};
