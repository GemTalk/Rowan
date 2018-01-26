"
The MethodRegistry maps GsNMethods to LoadedMethods.
"
Class {
	#name : 'RwGsLoadedSymbolDictMethod',
	#superclass : 'RwLoadedMethod',
	#classVars : [
		'MethodRegistry'
	],
	#category : 'Rowan-GemStone'
}

{ #category : 'instance creation' }
RwGsLoadedSymbolDictMethod class >> forMethod: aGsNMethod [

	| instance |
	instance := self newNamed: aGsNMethod selector asString.
	instance
		handle: aGsNMethod;
		updateIsMetaFromImage;
		registerYourself.
	^instance
]

{ #category : 'private' }
RwGsLoadedSymbolDictMethod >> definingPackageOfClass [

	^loadedClass loadedPackage
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictMethod >> key [
	"Answer some token that will uniquely identify me relative to any other LoadedMethod in the same LoadedClass."

	^name	"This is inadequate because the same selector can be defined in both class and metaclass."
]

{ #category : 'private-updating' }
RwGsLoadedSymbolDictMethod >> moveToPackageNamed: newPackageName [
	"I'm being moved from one package to another, or I'm new and being moved into a package.
 	I may need to be in a class extension.
	We may have to create a new LoadedClass, LoadedClassExtension, and/or a new package."

	| newPackage isExtension myClass newLoadedClass |
	loadedClass ifNotNil: [loadedClass removeLoadedMethod: self].	"That takes care of removal from any old package."

	"Now we need something to add ourselves to."
	newPackage := Rowan existingOrNewLoadedPackageNamed: newPackageName.
	myClass := self myNonMetaClass.
	isExtension := newPackage ~= self definingPackageOfClass.
	newLoadedClass := isExtension
				ifTrue: [newPackage loadedExtensionForClass: myClass]
				ifFalse: [newPackage existingLoadedClassForClass: myClass].
	newLoadedClass addLoadedMethod: self
]

{ #category : 'private' }
RwGsLoadedSymbolDictMethod >> myNonMetaClass [

	^handle inClass thisClass
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictMethod >> source [

	^handle sourceString copy
]

{ #category : 'private-updating' }
RwGsLoadedSymbolDictMethod >> updateForPackageChangeFrom: oldPackageName to: newPackageName newIsExtension: newIsExtension [
	"I'm being moved from one package to another. One or both of these is through a class extension.
	May have to create a new LoadedClass, LoadedExtendedClass, and/or a new package."
	"We know what our old loadedClass or loadedClassExtension is, so removing is simple."

	| newPackage myBehavior myClass packageOfClass newLoadedClass |
	loadedClass ifNotNil: [loadedClass removeLoadedMethod: self].

	"Now we need something to add ourselves to."
	newPackage := Rowan existingOrNewLoadedPackageNamed: newPackageName.
	myBehavior := handle inClass.
	classIsMeta := myBehavior isMeta.
	myClass := myBehavior thisClass.



	newLoadedClass := newIsExtension ifTrue: [] ifFalse: [].
	newLoadedClass := RwGsLoadedClass existingForClass: myClass.
	packageOfClass := newLoadedClass loadedPackage.
	newPackage = packageOfClass
		ifFalse: 
			["We're moving into a class extension, not the class definition.
			 Might have to create the extension."
			newLoadedClass := newPackage loadedExtensionForClass: myClass.
			newLoadedClass == AbsentToken
				ifTrue: 
					["Create an extension"
					newLoadedClass := RwGsLoadedClassExtension newNamed: myClass name.
					newPackage addLoadedClassExtension: newLoadedClass]].
	loadedClass := newLoadedClass
]

{ #category : 'private-updating' }
RwGsLoadedSymbolDictMethod >> updateForProtocolChange [
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
RwGsLoadedSymbolDictMethod >> updateForProtocolChangeTo: newProtocol [
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

{ #category : 'private-updating' }
RwGsLoadedSymbolDictMethod >> updateIsMetaFromImage [

	classIsMeta := handle inClass isMeta
]
