Class {
	#name : 'RwRowanProjectIssuesTest',
	#superclass : 'RwBrowserToolTest',
	#category : 'Rowan-Tests'
}

{ #category : 'private' }
RwRowanProjectIssuesTest class >> _symbolDictionaryNames [

	^ 	super _symbolDictionaryNames, 
			#( #'RowanSample4SymbolDict' #'SampleSymbolDict')
]
