Extension { #name : 'GsNMethod' }

{ #category : '*rowan-gemstone-kernel-stubs-37x' }
GsNMethod >> _rwRecompileFromSourceIfUnpackagedDo: unpackagedBlock [
	"If the method is packaged, then we need to force a recompile of the method, 
		since method source is not being changed"

	| packageName |
	packageName := self rowanPackageName.
	packageName = Rowan unpackagedName
		ifFalse: [
			"method is packaged, so force the method to be recompiled ... remember that the source is identical"
			| theClass loadedMethod |
			theClass := self inClass.
			loadedMethod := Rowan image
				loadedMethodForMethod: self.
			Rowan projectTools browser
				forceRecompileLoadedMethod: loadedMethod
				dictionaries: GsCurrentSession currentSession symbolList
				forClassNamed: theClass name asString
				isMeta: theClass isMeta ].
	^ unpackagedBlock value
]
