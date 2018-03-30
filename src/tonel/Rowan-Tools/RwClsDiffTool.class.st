Class {
	#name : 'RwClsDiffTool',
	#superclass : 'RwClassTool',
	#category : 'Rowan-Tools'
}

{ #category : 'smalltalk api' }
RwClsDiffTool >> patchClass: aClass1 with: aClass2 [

	| patch classDef1 package1 classDef2 package2 |
	classDef1 := (RwGsLoadedClass existingForClass: aClass1) asDefinition.
	package1 := RwPackageDefinition
		withProperties: (Dictionary with: 'name' -> aClass1 name)
		classDefinitions: (Dictionary new at: classDef1 key put: classDef1)
		classExtensions: Dictionary new.
	classDef2 := (RwGsLoadedClass existingForClass: aClass2) asDefinition.
	package2 := RwPackageDefinition
		withProperties: (Dictionary with: 'name' -> aClass2 name)
		classDefinitions: (Dictionary new at: classDef2 key put: classDef2)
		classExtensions: Dictionary new.
	^ CypressPatch fromBase: package2 toTarget: package1
]
