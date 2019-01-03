Class {
	#name : 'RwGsLoadedSymbolDictComponentProject',
	#superclass : 'RwGsLoadedSymbolDictProject',
	#instVars : [
		'loadedComponents'
	],
	#category : 'Rowan-GemStone-Components'
}

{ #category : 'definitions' }
RwGsLoadedSymbolDictComponentProject >> asDefinition [

	"Create a Definition corresponding to what I define in the image."

	^ RwComponentProjectDefinition
		withProperties: self propertiesForDefinition
		packageDefinitions: self loadedPackageDefinitions
		componentDefinitions: self loadedComponentDefinitions
]

{ #category : 'initialization' }
RwGsLoadedSymbolDictComponentProject >> initialize [

	super initialize.
	loadedComponents := KeyValueDictionary new.
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictComponentProject >> loadedComponentDefinitions [

	"Create definitions from all of the components I define, and answer the collection of them"

	^ self definitionsFor: loadedComponents
]
