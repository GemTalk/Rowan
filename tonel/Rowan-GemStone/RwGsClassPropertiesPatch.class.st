"
No class-specific documentation for CypGsClassPropertiesPatch, hierarchy is: 
Object
  CypGsPatch
    CypGsClassPatch( classDefinition packageDefinition)
      CypGsClassPropertiesPatch( propertiesModification)

"
Class {
	#name : 'RwGsClassPropertiesPatch',
	#superclass : 'RwGsClassPatch',
	#instVars : [
		'propertiesModification'
	],
	#category : 'Rowan-GemStone'
}

{ #category : 'installing' }
RwGsClassPropertiesPatch >> installPropertiesPatchNewClasses: createdClasses andExistingClasses: tempSymbols [
	" update class and update loadedClass with new properties"

	| className existingClass loadedClass |
	className := classDefinition key asSymbol.
	existingClass := createdClasses at: className
		ifAbsent: 
			[tempSymbols 
				at: className
				ifAbsent: [self error: 'Cannot find class to update properties for.']].
	loadedClass := RwGsLoadedClass existingForClass: existingClass.

self halt

]
