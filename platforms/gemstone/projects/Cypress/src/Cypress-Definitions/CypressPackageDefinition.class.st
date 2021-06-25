"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressPackageDefinition',
	#superclass : 'CypressObject',
	#instVars : [
		'name'
	],
	#category : 'Cypress-Definitions'
}

{ #category : 'instance creation' }
CypressPackageDefinition class >> named: aString [

	^self new
		name: aString;
		yourself.

]

{ #category : 'comparing' }
CypressPackageDefinition >> = other [
	^ other species = self species and: [other name sameAs: name]

]

{ #category : 'snapshotting' }
CypressPackageDefinition >> addClass: aClass toDefinitions: definitions [

	definitions add: (CypressClassDefinition forClass: aClass)

]

{ #category : 'snapshotting' }
CypressPackageDefinition >> addExtensionMethodsFromClass: aClass toMap: classMap [

	| defs map |
	defs := classMap at: aClass theNonMetaClass
				ifAbsent: [OrderedCollection new].
	map := Dictionary new.
	aClass categorysDo: 
			[:category :selectors |
			(category asLowercase
				indexOfSubCollection: '*' , self basePackageName asLowercase startingAt: 1 ifAbsent: [ 0 ]) = 1
				ifTrue: [map at: category put: selectors asSortedCollection]].
	map keys asSortedCollection do: 
			[:category |
			(map at: category) do: 
					[:selector |
					defs add: (CypressMethodDefinition
								forMethod: (aClass compiledMethodAt: selector))]].
	defs notEmpty ifTrue: [classMap at: aClass theNonMetaClass put: defs]

]

{ #category : 'snapshotting' }
CypressPackageDefinition >> addMethodsFromClass: aClass toDefinitions: definitions [
	"Add only those methods which are not extensions from other packages."

	(((aClass methodDictForEnv: 0)
		reject: [:each | (each inClass categoryOfSelector: each selector) first = $*])
			asSortedCollection: [:a :b | a selector <= b selector])
			do: [:method | definitions add: (CypressMethodDefinition forMethod: method)]

]

{ #category : 'accessing' }
CypressPackageDefinition >> basePackageName [
  "package name may have a platform/branch extension, when comparing against category/protocol names, extension is ignored"

  | nm index |
  nm := self name.
  index := nm indexOfSubCollection: '.' startingAt: 1.
  index = 0
    ifTrue: [ ^ nm ].
  ^ nm copyFrom: 1 to: index - 1

]

{ #category : 'accessing' }
CypressPackageDefinition >> classes [
  ^ self classesInPackageNamed: self basePackageName

]

{ #category : 'comparing' }
CypressPackageDefinition >> hash [
  ^ name hash

]

{ #category : 'accessing' }
CypressPackageDefinition >> name [
	^ name

]

{ #category : 'accessing' }
CypressPackageDefinition >> name: aString [
	name := aString

]

{ #category : 'printing' }
CypressPackageDefinition >> printDetailsOn: aStream [

	aStream nextPutAll: name

]

{ #category : 'snapshotting' }
CypressPackageDefinition >> snapshot [

	| classDefinitions methodDefinitions classMap |
	classDefinitions := OrderedCollection new.
	methodDefinitions := OrderedCollection new.
	(self determineClassHierarchicalOrder: self classes) do: 
			[:cls |
			self
				addClass: cls toDefinitions: classDefinitions;
				addMethodsFromClass: cls toDefinitions: methodDefinitions;
				addMethodsFromClass: cls class toDefinitions: methodDefinitions].
	classMap := Dictionary new.
	self allClasses do: 
			[:each |
			self
				addExtensionMethodsFromClass: each toMap: classMap;
				addExtensionMethodsFromClass: each class toMap: classMap].
	(self determineClassHierarchicalOrder: classMap keys)
		do: [:aClass | methodDefinitions addAll: (classMap at: aClass)].
	^CypressSnapshot definitions: classDefinitions, methodDefinitions

]
