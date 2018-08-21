! Package: STON-GemStoneBase-Core


! Remove existing behavior from package STON-GemStoneBase-Core
!!!! This can be cleaned up when some package functionality is moved to the base system.

doit
| packageName |
packageName := 'STON-GemStoneBase'.
System myUserProfile symbolList do: [:symDict |
	symDict do: [:possibleClass |
			| toRemove |
		possibleClass isBehavior ifTrue: [
			{possibleClass. possibleClass class} do: [:aClass |
				aClass category = packageName
					ifTrue: [
							"*anythingbutpackagename[-anything]"
						toRemove := aClass categoryNames select: 
										[:each |
										each isEmpty not and: [
											(each first = $* and: [(each size = (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2])
														or: [each size > (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2 and: [(each at: packageName size + 2) = $-]]]])
											or: [each first ~= $*]]]
					]
					ifFalse: [
							"*packagename[-anything]"
						toRemove := aClass categoryNames select: 
										[:each |
										each isEmpty not and: [
											each first = $* and: [(each size = (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2])
														or: [each size > (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2 and: [(each at: packageName size + 2) = $-]]]]]]
					].
				toRemove do: [:each | aClass removeCategory: each].
			]
		]
	]
].
true.
%


! Class Declarations

! Class Extensions

! Class Extension for ByteArray

! ------------------- Instance methods for ByteArray

category: '*ston-gemstonebase'
method: ByteArray
readHexFrom: aStream
  "Initialize the receiver from a hexadecimal string representation"

  | map v ch value |
  map := '0123456789abcdefABCDEF'.
  1 to: self size do: [ :i | 
    ch := aStream next.
    v := (map indexOf: ch) - 1.
    ((v between: 0 and: 15) or: [ (v := v - 6) between: 0 and: 15 ])
      ifFalse: [ 
        ^ self
          error:
            'Hex digit 
expected' ].
    value := v bitShift: 4.
    ch := aStream next.
    v := (map indexOf: ch) - 1.
    ((v between: 0 and: 15) or: [ (v := v - 6) between: 0 and: 15 ])
      ifFalse: [ 
        ^ self
          error:
            'Hex digit 
expected' ].
    value := value + v.
    self at: i put: value ]
%

! Class Extension for CharacterCollection

! ------------------- Instance methods for CharacterCollection

category: '*ston-gemstonebase'
method: CharacterCollection
isString
  ^ true
%

! Class Extension for Object

! ------------------- Instance methods for Object

category: '*ston-gemstonebase'
method: Object
isNumber
  ^ self _isNumber
%

category: '*ston-gemstonebase'
method: Object
isString
  ^ false
%

! Class Extension for SequenceableCollection

! ------------------- Class methods for SequenceableCollection

category: '*STON-GemStoneBase'
classmethod: SequenceableCollection
new: newSize streamContents: blockWithArg
  | stream |
  stream := WriteStreamPortable on: (self new: newSize).
  blockWithArg value: stream.
  ^ stream contents
%

category: '*STON-GemStoneBase'
classmethod: SequenceableCollection
streamContents: blockWithArg
  ^ self new: 100 streamContents: blockWithArg
%

! Class Extension for STONReader

! ------------------- Class methods for STONReader

category: '*ston-gemstonebase'
classmethod: STONReader
new
  ^ self basicNew
    initialize;
    yourself
%

! ------------------- Instance methods for STONReader

category: '*ston-gemstonebase'
method: STONReader
lookupClass: name
  ^ (System myUserProfile objectNamed: name asSymbol)
    ifNil: [ 
		(((AllUsers userWithId: 'SystemUser') objectNamed: 'RowanTools')
			ifNotNil: [:rowanSymbolDictionary |
				(rowanSymbolDictionary at: name asSymbol ifAbsent: [])
					ifNotNil: [:cls | ^cls ] ])
						ifNil: [ classes at: name ifAbsentPut: [ (ClassOrganizer new allSubclassesOf: Object)
								detect: [ :cls | cls stonName == name ]
								ifNone: [
									(((AllUsers userWithId: 'SystemUser') objectNamed: 'Rowan') 
										platform serviceClassFor: name)
											ifNil: [ self error: 'Cannot resolve class named ' , name printString ] ] ] ] ]
%

! Class Extension for STONStreamWriter

! ------------------- Class methods for STONStreamWriter

category: '*ston-gemstonebase'
classmethod: STONStreamWriter
new
  ^ self basicNew
    initialize;
    yourself
%

! Class Extension for STONWriter

! ------------------- Class methods for STONWriter

category: '*ston-gemstonebase'
classmethod: STONWriter
new
  ^ self basicNew
    initialize;
    yourself
%

! ------------------- Instance methods for STONWriter

category: '*ston-gemstonebase'
method: STONWriter
writeFloat: float
  writeStream nextPutAll: float asString
%

! Class initializers 

doit
true.
%



! End of Package: STON-GemStoneBase-Core


