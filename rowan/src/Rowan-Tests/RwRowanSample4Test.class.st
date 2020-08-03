Class {
	#name : 'RwRowanSample4Test',
	#superclass : 'RwRowanSampleAbstractTest',
	#category : 'Rowan-Tests'
}

{ #category : 'private' }
RwRowanSample4Test class >> _symbolDictionaryNames [

	^ 	super _symbolDictionaryNames, 
			#( #'RowanSample4SymbolDict' #'RowanSample4DictionarySymbolDict' #'RowanSample4DictionarySymbolDict_295'
					#'RowanSample4DictionarySymbolDict_295_3' #'SampleSymbolDict')
]

{ #category : 'private' }
RwRowanSample4Test >> _rowanSample4_200_LoadSpecificationUrl [

	| rowanProject |
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	^ 'file:' , rowanProject repositoryRootPath , '/test/specs/RowanSample4_200_load.ston'
]

{ #category : 'running' }
RwRowanSample4Test >> setUp [

	super setUp.
	Transcript cr; show: self printString
]
