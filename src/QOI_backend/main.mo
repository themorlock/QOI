import QOI "QOI";
import Result "mo:base/Result";

actor {
  public func encodeBitmap(input : QOI.SharedBitmap) : async Result.Result<QOI.SharedQOI, ()> {
    let value = QOI.encode(QOI.fromSharedBitmap(input));
    switch (value) {
      case (#ok(x)) {
        return #ok(QOI.toSharedQOI(x));
      };
      case (#err()) {
        return #err(());
      };
    };
  };

  public func decodeQOI(input : QOI.SharedQOI) : async Result.Result<QOI.SharedBitmap, ()> {
    let value = QOI.decode(QOI.fromSharedQOI(input));
    switch (value) {
      case (#ok(x)) {
        return #ok(QOI.toSharedBitmap(x));
      };
      case (#err()) {
        return #err(());
      };
    };
  };
};
