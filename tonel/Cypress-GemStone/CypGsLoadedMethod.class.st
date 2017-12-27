"
The MethodRegistry maps GsNMethods to LoadedMethods.
"
Class {
	#name : 'CypGsLoadedMethod',
	#superclass : 'CypLoadedMethod',
	#classVars : [
		'MethodRegistry'
	],
	#category : 'Cypress-GemStone'
}

{ #category : 'enumeration' }
CypGsLoadedMethod class >> allDoSafely: aBlock [
	"Evaluate aBlock for every registered LoadedMethod in the system. 
	The block may modify the MethodRegistry, so we must copy the values first."

	MethodRegistry values do: aBlock
]

{ #category : 'enumeration' }
CypGsLoadedMethod class >> allMethodsDo: aBlock [
	"Iterate over all methods defined in classes visible in the current session's transient symbol list, 
		or in the corresponding metaclass, evaluating the block with each."

	CypGsLoadedClass allClassesDo: 
			[:class |
			self
				allMethodsInBehavior: class do: aBlock;
				allMethodsInBehavior: class class do: aBlock]
]

{ #category : 'enumeration' }
CypGsLoadedMethod class >> allMethodsInBehavior: behavior do: aBlock [
	"Iterate over all methods defined in the given behavior, evaluating the block with each."

	behavior selectors
		do: [:selector | aBlock value: (behavior compiledMethodAt: selector)]
]

{ #category : 'queries' }
CypGsLoadedMethod class >> existingForMethod: aMethod [
	"Answer the registered LoadedMethod for the given method."

	^MethodRegistry at: aMethod ifAbsent: [AbsentToken]
]

{ #category : 'queries' }
CypGsLoadedMethod class >> existingForMethod: aMethod ifAbsent: absentBlock [
	"Answer the registered LoadedMethod for the given method."

	^MethodRegistry at: aMethod ifAbsent: absentBlock
]

{ #category : 'instance creation' }
CypGsLoadedMethod class >> forMethod: aGsNMethod [

	| instance |
	instance := self newNamed: aGsNMethod selector asString.
	instance
		handle: aGsNMethod;
		updateIsMetaFromImage;
		registerYourself.
	^instance
]

{ #category : 'class initialization' }
CypGsLoadedMethod class >> initialize [
	"CypGsLoadedMethod initialize"

	MethodRegistry := KeyValueDictionary new
]

{ #category : 'private' }
CypGsLoadedMethod >> definingPackageOfClass [

	^loadedClass loadedPackage
]

{ #category : 'private-updating' }
CypGsLoadedMethod >> handleMethodDeletionOrReplacement [
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
]

{ #category : 'accessing' }
CypGsLoadedMethod >> key [
	"Answer some token that will uniquely identify me relative to any other LoadedMethod in the same LoadedClass."

	^name	"This is inadequate because the same selector can be defined in both class and metaclass."
]

{ #category : 'private-updating' }
CypGsLoadedMethod >> moveToPackageNamed: newPackageName [
	"I'm being moved from one package to another, or I'm new and being moved into a package.
 	I may need to be in a class extension.
	We may have to create a new LoadedClass, LoadedClassExtension, and/or a new package."

	| newPackage isExtension myClass newLoadedClass |
	loadedClass ifNotNil: [loadedClass removeLoadedMethod: self].	"That takes care of removal from any old package."

	"Now we need something to add ourselves to."
	newPackage := Cypress existingOrNewLoadedPackageNamed: newPackageName.
	myClass := self myNonMetaClass.
	isExtension := newPackage ~= self definingPackageOfClass.
	newLoadedClass := isExtension
				ifTrue: [newPackage loadedExtensionForClass: myClass]
				ifFalse: [newPackage existingLoadedClassForClass: myClass].
	newLoadedClass addLoadedMethod: self
]

{ #category : 'private' }
CypGsLoadedMethod >> myNonMetaClass [

	^handle inClass thisClass
]

{ #category : 'private' }
CypGsLoadedMethod >> registerYourself [

	(MethodRegistry includesKey: handle)
		ifTrue: [self error: 'Method already registered.'].
	MethodRegistry at: handle put: self
]

{ #category : 'accessing' }
CypGsLoadedMethod >> source [

	^handle sourceString copy
]

{ #category : 'private-updating' }
CypGsLoadedMethod >> updateForPackageChangeFrom: oldPackageName to: newPackageName newIsExtension: newIsExtension [
	"I'm being moved from one package to another. One or both of these is through a class extension.
	May have to create a new LoadedClass, LoadedExtendedClass, and/or a new package."
	"We know what our old loadedClass or loadedClassExtension is, so removing is simple."

	| newPackage myBehavior myClass packageOfClass newLoadedClass |
	loadedClass ifNotNil: [loadedClass removeLoadedMethod: self].

	"Now we need something to add ourselves to."
	newPackage := Cypress existingOrNewLoadedPackageNamed: newPackageName.
	myBehavior := handle inClass.
	classIsMeta := myBehavior isMeta.
	myClass := myBehavior thisClass.



	newLoadedClass := newIsExtension ifTrue: [] ifFalse: [].
	newLoadedClass := CypGsLoadedClass existingForClass: myClass.
	packageOfClass := newLoadedClass loadedPackage.
	newPackage = packageOfClass
		ifFalse: 
			["We're moving into a class extension, not the class definition.
			 Might have to create the extension."
			newLoadedClass := newPackage loadedExtensionForClass: myClass.
			newLoadedClass == AbsentToken
				ifTrue: 
					["Create an extension"
					newLoadedClass := CypGsLoadedClassExtension newNamed: myClass name.
					newPackage addLoadedClassExtension: newLoadedClass]].
	loadedClass := newLoadedClass
]

{ #category : 'private-updating' }
CypGsLoadedMethod >> updateForProtocolChange [
	"Check to see whether I'm in a different protocol. If I am, that could change which package I'm in and I need to update that."

	| oldProtocol newProtocol |
	oldProtocol := self propertyAt: 'protocol'.
	newProtocol := (handle inClass categoryOfSelector: handle selector)
				asString.

	"Must test for AbsentToken by identity, or a new protocol of '<absent>' would not  be detected."
	oldProtocol == AbsentToken | (oldProtocol ~= newProtocol)
		ifTrue: [self updateForProtocolChangeTo: newProtocol]
]

{ #category : 'private-updating' }
CypGsLoadedMethod >> updateForProtocolChangeTo: newProtocol [
	"My protocol has changed. This might also mean that my package has changed."

	| currentPackageName newPackageName |
	self propertyAt: 'protocol' put: newProtocol.
	currentPackageName := loadedClass packageName.
	newProtocol first = $*
		ifTrue: [newPackageName := newProtocol copyFrom: 2 to: newProtocol size]
		ifFalse: [newPackageName := self definingPackageOfClass name].
	currentPackageName = newPackageName
		ifFalse: [self moveToPackageNamed: newPackageName]
]

{ #category : 'updating' }
CypGsLoadedMethod >> updateFromImageProtocols [
	"My information about a method may be out of date. Update it.
	Things that might have caused me to need updating:
	* There may no longer be a method with my name (selector) defined in the class.
	* There may be a different method defined with my name.
	* The method may be in a different protocol.
	* If the protocol has changed, the method may be in a new package."

	self handleMethodDeletionOrReplacement
		ifTrue: [self updateForProtocolChange]
		ifFalse: 
			["Method has been deleted and the deletion handled."
			]
]

{ #category : 'private-updating' }
CypGsLoadedMethod >> updateIsMetaFromImage [

	classIsMeta := handle inClass isMeta
]
