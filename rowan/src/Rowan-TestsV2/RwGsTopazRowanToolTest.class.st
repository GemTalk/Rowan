Class {
	#name : 'RwGsTopazRowanToolTest',
	#superclass : 'RwAbstractV2Test',
	#category : 'Rowan-TestsV2'
}

{ #category : 'private' }
RwGsTopazRowanToolTest class >> _symbolDictionaryNames [
	^ super _symbolDictionaryNames , {'RowanSample9Class1'}
]

{ #category : 'private' }
RwGsTopazRowanToolTest >> _loadSpecNamed: specName [
""
	^ RwRowanSample9Test _loadSpecNamed: specName
]

{ #category : 'tonel support' }
RwGsTopazRowanToolTest >> testTopazReadTonelFile [
	"load spec_0001 and then checkout spec_0009 and read 
		rowan/src/RowanSample9-Extensions/RowanSample9Class1.extension.st, which adds a class
		method #bar"

	| loadSpec projectName projectNames resolvedProject loadedProjects project className class |
	loadSpec := self _loadSpecNamed: 'spec_0001'.

	projectName := loadSpec projectName.
	projectNames := {projectName}.
	className := projectName , 'Class1'.

	projectNames
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

"resolve project"
	resolvedProject := loadSpec resolve.

"load project"
	loadedProjects := resolvedProject load.
	project := loadedProjects at: 1.

"validate"
	class := Rowan globalNamed: className.
	self assert: (class compiledMethodAt: #foo otherwise: nil) notNil.
	self assert: class new foo = 1.
	self assert: (class class compiledMethodAt: #bar otherwise: nil) isNil.

"read tonel file from spec_0009"
	project checkout: 'spec_0009'.
	Rowan gemstoneTools topaz topazReadTonelFile: project repositoryRoot / 'rowan' / 'src' / 'RowanSample9-Extensions' / (className, '.extension.st').

"validate"
	self assert: (class compiledMethodAt: #foo otherwise: nil) notNil.
	self assert: class new foo = 1.
	self assert: (class class compiledMethodAt: #bar otherwise: nil) notNil.
	self assert: class bar = 'bar'
]
