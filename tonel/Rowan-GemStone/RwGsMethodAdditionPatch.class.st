"
No class-specific documentation for CypGsMethodAdditionPatch, hierarchy is: 
Object
  CypGsPatch
    CypGsMethodAdditionPatch( isMeta methodDefinition classDefinition packageDefinition)

"
Class {
	#name : 'RwGsMethodAdditionPatch',
	#superclass : 'RwGsMethodPatch',
	#category : 'Rowan-GemStone'
}

{ #category : 'private-installing' }
RwGsMethodAdditionPatch >> createLoadedMethod [

	| existing new |
	existing := RwGsLoadedMethod existingForMethod: compiledMethod
				ifAbsent: [nil].
	existing
		ifNotNil: 
			[self
				error: 'Internal error -- existing LoadedMethod found for new method.'].
	new := RwGsLoadedMethod newNamed: selector.
	new
		handle: compiledMethod;
		classIsMeta: isMeta;
		registerYourself.
	^new
]

{ #category : 'private-installing' }
RwGsMethodAdditionPatch >> installLoadedMethod [

	| loadedMethod loadedClassOrExtension |
	loadedMethod := self createLoadedMethod.
	loadedClassOrExtension := self loadedClassOrExtensionForMethod.
	loadedClassOrExtension addLoadedMethod: loadedMethod.
	loadedMethod setPropertiesTo: methodDefinition properties
]

{ #category : 'installing' }
RwGsMethodAdditionPatch >> installMethod [

	self
		updateMethodDictionary;
		setProtocol;
		installLoadedMethod
]

{ #category : 'private-testing' }
RwGsMethodAdditionPatch >> isForExtensionMethod [
	"Answer true if the method is not in the same package that defines the class."

	| packageOfClass packageOfMethod loadedClass |
	packageOfMethod := packageDefinition key.
	loadedClass := RwGsLoadedClass existingForClass: behavior thisClass.
	packageOfClass := loadedClass loadedPackage name.
	^packageOfClass ~= packageOfMethod
]

{ #category : 'private-installing' }
RwGsMethodAdditionPatch >> packageProtocolName [
	"If I'm for an extension method, my protocol in the image may need to be *<packageName>*."

	^'*' , packageDefinition key
]

{ #category : 'private-installing' }
RwGsMethodAdditionPatch >> propertiesProtocolName [

	^methodDefinition propertyAt: 'protocol' ifAbsent: ['unknown']
]

{ #category : 'private-installing' }
RwGsMethodAdditionPatch >> setProtocol [
	"If I'm an extension method, my protocol in the image may need to be *<packageName>."

	self
		setProtocolTo: (Rowan configuration needsPackageProtocolNames
				& self isForExtensionMethod
					ifTrue: [self packageProtocolName]
					ifFalse: [self propertiesProtocolName])
]

{ #category : 'private-installing' }
RwGsMethodAdditionPatch >> setProtocolTo: protocolString [

	| protocolSymbol |
	protocolSymbol := protocolString asSymbol.
	(behavior includesCategory: protocolSymbol)
		ifFalse: [behavior addCategory: protocolSymbol].
	behavior moveMethod: selector toCategory: protocolSymbol
]

{ #category : 'private-installing' }
RwGsMethodAdditionPatch >> updateMethodDictionary [

	| methodDictionary |
	methodDictionary := behavior persistentMethodDictForEnv: 0.
	selector := compiledMethod selector.
	methodDictionary at: selector put: compiledMethod
]
