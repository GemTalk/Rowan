"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private-updating
"
handleMethodDeletionOrReplacement
	"If the class to which my loadedClass refers no longer contains a method with my name (selector), the method has been deleted
	If the method is still there, check whether it's been replaced, and answer true whether it has or not. 
	If the method has been deleted, remove myself and answer false."

	| theClass theBehavior theMethod |
	theClass := loadedClass handle.
	theBehavior := classIsMeta ifTrue: [theClass class] ifFalse: [theClass].
	theMethod := theBehavior compiledMethodAt: name asSymbol otherwise: nil.
	^theMethod
		ifNil: 
			["Deleted, delete myself"
			loadedClass removeLoadedMethod: self.
			MethodRegistry removeKey: handle.
			false]
		ifNotNil: 
			["Check for replaced method"
			theMethod == handle
				ifFalse: 
					[self markPackageDirty.
					handle := theMethod].
			true]