Class {
	#name : 'JadeServer64bit32',
	#superclass : 'JadeServer64bit3x',
	#category : 'Rowan-JadeServer'
}

{ #category : 'category' }
JadeServer64bit32 >> dictionaryAndSymbolOf: aClass [

	| array |
	array := self symbolList dictionariesAndSymbolsOf: aClass.
	^array isEmpty
		ifTrue: [nil]
		ifFalse: [array first].


]

{ #category : 'category' }
JadeServer64bit32 >> dictionaryAndSymbolOf: aClass forUser: aUserProfile [

	| array |
	array := aUserProfile symbolList dictionariesAndSymbolsOf: aClass.
	^array isEmpty
		ifTrue: [nil]
		ifFalse: [array first].


]

{ #category : 'category' }
JadeServer64bit32 >> gsPackagePolicy [

	| class |
	class := self gsPackagePolicyClass.
	class isNil ifTrue: [^nil].
	class enabled ifFalse: [^nil].
	^class current


]

{ #category : 'category' }
JadeServer64bit32 >> stepThrough: aGsProcess inFrame: anInteger [
  aGsProcess _stepThrough
]
