Extension { #name : 'ByteArray' }

{ #category : '*filesystem-gemstone-kernel' }
ByteArray >> asString [
  "Convert to a String with Characters for each byte"

  ^ String withBytes: self

]

{ #category : '*filesystem-gemstone-kernel' }
ByteArray >> decodeWith: encoding [
	"Produce a String that decodes the receiver, using a specified encoding.
	Encoding is either a ZnCharacterEncoder instance or an identifier for one."
	
	"#[76 101 115 32 195 169 108 195 168 118 101 115 32 102 114 97 110 195 167 97 105 115] decodeWith: #utf8"
	
	^ encoding asZnCharacterEncoder decodeBytes: self

]

{ #category : '*filesystem-gemstone-kernel' }
ByteArray >> utf8Decoded [
	"Produce a String decoding the receiver using UTF-8,
	the recommended encoding for Strings, unless you know what you are doing."

	"#[76 101 115 32 195 169 108 195 168 118 101 115 32 102 114 97 110 195 167 97 105 115] utf8Decoded"
	
	^ self asUnicodeString

]

{ #category : '*filesystem-gemstone-kernel' }
ByteArray class >> readHexFrom: aString [
  "Create a byte array from a hexadecimal representation"

  ^ (self new: aString size // 2) readHexFrom: aString readStream

]
