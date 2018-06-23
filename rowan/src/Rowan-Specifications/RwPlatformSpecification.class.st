Class {
	#name : 'RwPlatformSpecification',
	#superclass : 'RwSpecification',
	#category : 'Rowan-Specifications'
}

{ #category : 'initialization' }
RwPlatformSpecification >> initializeForExport [
  "if spec has been exported, clear out any of the fields that represent local disk state"

	super initializeForExport

]

{ #category : 'initialization' }
RwPlatformSpecification >> initializeForImport [

  "if spec has been imported, clear out any of the fields that represent local disk state"
  
	super initializeForImport

]

{ #category : 'copying' }
RwPlatformSpecification >> postCopy [
  "don't forget to copy any compound state objects"

  super postCopy
]
