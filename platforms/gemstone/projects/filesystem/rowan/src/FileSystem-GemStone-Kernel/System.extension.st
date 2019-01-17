Extension { #name : 'System' }

{ #category : '*FileSystem-GemStone-Kernel' }
System class >> clientOsName [
	^self clientVersionReport at: 'osName'

]

{ #category : '*FileSystem-GemStone-Kernel' }
System class >> lineEnding [
  "Answer the os-specific line endings"

  ^ String with: Character lf

]

{ #category : '*FileSystem-GemStone-Kernel' }
System class >> osName [
	^self gemVersionAt: 'osName'
]
