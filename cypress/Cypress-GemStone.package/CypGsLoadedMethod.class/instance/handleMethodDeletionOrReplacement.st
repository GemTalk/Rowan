"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private-updating
"
handleMethodDeletionOrReplacement
	"The method has been deleted if the class to which my loadedClass refers no longer contains a method with my name (selector).
	If the method is still there, check whether it's been replaced, and answer true. If not, delete myself and answer false."

	| theClass theMethod |
	theClass := loadedClass handle.
	theMethod := theClass compiledMethodAt: name asSymbol otherwise: nil.
	^theMethod
		ifNil: 
			["Deleted, delete myself"
			self markPackageDirty.
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