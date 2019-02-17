"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressSnapshot',
	#superclass : 'CypressObject',
	#instVars : [
		'definitions'
	],
	#category : 'Cypress-Definitions'
}

{ #category : 'instance creation' }
CypressSnapshot class >> definitions: aDefinitions [

	^(self new) definitions: aDefinitions

]

{ #category : 'instance creation' }
CypressSnapshot class >> empty [

  ^self definitions: #()

]

{ #category : 'comparing' }
CypressSnapshot >> = other [
	^ definitions asArray = other definitions asArray

]

{ #category : 'enumerating' }
CypressSnapshot >> classDefinitions: classBlock methodDefinitions: methodBlock [

	self definitions do: [:definition |
		definition classDefinition: classBlock methodDefinition: methodBlock]

]

{ #category : 'accessing' }
CypressSnapshot >> definitions [

	^definitions

]

{ #category : 'accessing' }
CypressSnapshot >> definitions: aDefinitions [

	definitions := aDefinitions

]

{ #category : 'comparing' }
CypressSnapshot >> hash [
  ^ definitions asArray hash

]

{ #category : 'patching' }
CypressSnapshot >> patchRelativeToBase: aSnapshot [
	^ CypressPatch fromBase: aSnapshot toTarget: self

]

{ #category : 'unloading' }
CypressSnapshot >> unload [

  ^CypressLoader unloadSnapshot: self

]

{ #category : 'loading' }
CypressSnapshot >> updatePackage: aPackage [
  "Answer the loader used to apply the update."

  ^ self updatePackage: aPackage defaultSymbolDictionaryName: nil

]

{ #category : 'loading' }
CypressSnapshot >> updatePackage: aPackage defaultSymbolDictionaryName: defaultSymbolDictionaryName [
  "Answer the loader used to apply the update."

  ^ CypressLoader
    updatePackage: aPackage
    defaultSymbolDictionaryName: defaultSymbolDictionaryName
    withSnapshot: self

]
