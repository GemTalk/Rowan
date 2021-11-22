Class {
	#name : 'PharoTonelV1STONWriter',
	#superclass : 'TonelSTONWriter',
	#category : 'Rowan-GemStone-Core'
}

{ #category : 'private' }
PharoTonelV1STONWriter >> encodeKey: key value: value [
	self nextPut: key asSymbol.
	self prettyPrintSpace.
	writeStream nextPut: $:.
	self prettyPrintSpace.
	self nextPut: value asSymbol
]
