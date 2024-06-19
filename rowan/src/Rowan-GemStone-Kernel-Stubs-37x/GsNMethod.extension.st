Extension { #name : 'GsNMethod' }

{ #category : '*rowan-gemstone-kernel-stubs-37x' }
GsNMethod >> _rwRecompileFromSourceIfUnpackagedDo: unpackagedBlock [
	"If the method is packaged, then we need to force a recompile of the method, 
		since method source is not being changed"

	| packageName |
	packageName := self rowanPackageName.
	packageName = Rowan unpackagedName
		ifFalse: [ 
			| theClass loadedMethod |
			"method is packaged, so force the method to be recompiled ... remember that the source is identical"
			theClass := self inClass.
			loadedMethod := Rowan image loadedMethodForMethod: self.
			Rowan projectTools browser
				forceRecompileLoadedMethod: loadedMethod
				dictionaries: GsCurrentSession currentSession symbolList
				forClassNamed: theClass name asString
				isMeta: theClass isMeta.
			^ self ].
	^ unpackagedBlock value
]

{ #category : '*rowan-gemstone-kernel-stubs-37x' }
GsNMethod >> _sourceOriginString [
  "Answer a string indicating the origin of the method: traitName or packageName (if Rowan 3 installed)"

	| loadedPackage |
	self isFromTrait
		ifTrue: [ ^ '---ORIGIN: Trait method ' , self origin name printString , '---' ].
	self isFromRowan
		ifFalse: [ ^ '' ].
	loadedPackage := self origin loadedPackage.
	^ '---ORIGIN: Rowan project ' , loadedPackage loadedProject name printString , ' package '
		, loadedPackage name printString , '---'
]
