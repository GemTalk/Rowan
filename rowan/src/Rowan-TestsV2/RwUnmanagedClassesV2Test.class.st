Class {
	#name : 'RwUnmanagedClassesV2Test',
	#superclass : 'RwAbstractV2Test',
	#category : 'Rowan-TestsV2'
}

{ #category : 'private' }
RwUnmanagedClassesV2Test >> _removeUnmanagedClass [
	| className |
	className := self _unmanagedClassName.

	(UserGlobals at: className asSymbol ifAbsent: [])
		ifNotNil: [ 
			[ Rowan projectTools browser removeClassNamed: className ]
				on: RwPerformingUnpackagedEditNotification
				do: [ :ex | ex resume ] ]
]

{ #category : 'private' }
RwUnmanagedClassesV2Test >> _unmanagedClassName [
	^ 'UnmanagedTestClass'
]

{ #category : 'running' }
RwUnmanagedClassesV2Test >> setUp [
	super setUp.
	self _removeUnmanagedClass
]

{ #category : 'running' }
RwUnmanagedClassesV2Test >> tearDown [
	self _removeUnmanagedClass.
	super tearDown.
]

{ #category : 'tests' }
RwUnmanagedClassesV2Test >> testUnmanagedClass [
	"should be able to use Rowan expression to package an unmanaged class"

	| unmanagedPackageName className unmanagedClass managedClass |
UserGlobals at: #ConditionalHalt put: false.
	unmanagedPackageName := Rowan unpackagedPackagePrefix , 'UserGlobals'.

	className := self _unmanagedClassName.

	(Rowan globalNamed: className)
		ifNotNil: [ 
			[ Rowan projectTools browse removeClassNamed: className ]
				on: RwPerformingUnpackagedEditNotification
				do: [ :ex | ex resume ] ].

	unmanagedClass := Object 
		subclass: className
		instVarNames: #(#'xxx')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: UserGlobals
		options: #().
	self assert: unmanagedClass notNil.
	self assert: (Rowan globalNamed: className) notNil.

	managedClass := Object
		rwSubclass: className
		instVarNames: #(#'xxx')
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: nil
		packageName: unmanagedPackageName
		constraints: #()
		options: #().

	self assert: unmanagedClass == managedClass.
]
