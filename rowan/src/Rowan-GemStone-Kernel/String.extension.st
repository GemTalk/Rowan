Extension { #name : 'String' }

{ #category : '*rowan-gemstone-kernel' }
String >> decodeFromUTF8 [

"Deprecated.  Applications should use instances of Utf8 to hold UTF-8 data.
 Decode receiver from UTF8 format."

"Undeprecated by Rowan, because the method has been undeprecated in later GemStone versions"

" Returns either a Unicode7 , Unicode16 or Unicode32 ,
 using the minimum character size needed to represent decoded result."

 ^ self _decodeFromUtf8: true

]
