Class {
	#name : 'RwGsMethodExtensionSessionMethodPropertiesSymDictPatch',
	#superclass : 'RwGsMethodPropertiesSymDictPatch',
	#category : 'Rowan-GemStone-Core'
}

{ #category : 'installing' }
RwGsMethodExtensionSessionMethodPropertiesSymDictPatch >> installPropertiesPatchNewClasses: createdClasses andExistingClasses: tempSymbols [

	" update method protocol and update loadedMethod with new compiled method"

	| dictsArray mDict cDict oldCompiledMethod |
	self primeBehaviorNewClasses: createdClasses andExistingClasses: tempSymbols.
	behavior
		ifNil: [ self error: 'Class ' , self className printString , ' not found.' ].

	dictsArray := self symbolDictionaryRegistry homeSessionMethods methodAndCategoryDictionaryFor: behavior.
	mDict := dictsArray at: 1.
	cDict := dictsArray at: 2.
		
	oldCompiledMethod :=  mDict 
		at: self propertiesSelector
		ifAbsent: [ 
			self
				error:
					'Internal error -- no existing CompileMethod found for patched method.' ].

	self symbolDictionaryRegistry
		moveSessionMethodCompiledMethod: oldCompiledMethod
		toProtocol: self propertiesProtocolName

]

{ #category : 'private-installing' }
RwGsMethodExtensionSessionMethodPropertiesSymDictPatch >> propertiesSelector [

	^methodDefinition propertyAt: 'selector' ifAbsent: ['unknown']

]
