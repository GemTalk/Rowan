"
There are not tests in this class for Rowan V2.0 ... still have tests floating around in post V1.2 work, so this class will hang out until no longer needed
"
Class {
	#name : 'RwRowanSample2Test',
	#superclass : 'RwRowanSampleAbstractTest',
	#category : 'Rowan-Tests'
}

{ #category : 'private' }
RwRowanSample2Test class >> _symbolDictionaryNames [
	^ super _symbolDictionaryNames , #(#'SampleSymbolDict'  #'RowanSample2' #'RowanSample2_SD')
]
