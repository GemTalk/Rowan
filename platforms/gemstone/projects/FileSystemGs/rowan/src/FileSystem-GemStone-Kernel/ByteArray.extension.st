Extension { #name : 'ByteArray' }

{ #category : '*filesystem-gemstone-kernel' }
ByteArray >> asString [
  "Convert to a String with Characters for each byte"

  ^ String withBytes: self

]

{ #category : '*filesystem-gemstone-kernel' }
ByteArray class >> readHexFrom: aString [
  "Create a byte array from a hexadecimal representation"

  ^ (self new: aString size // 2) readHexFrom: aString readStream

]
