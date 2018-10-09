Class {
	#name : 'JadeServerTest',
	#superclass : 'TestCase',
	#category : 'Rowan-Services-Tests'
}

{ #category : 'support' }
JadeServerTest >> jadeServer [
	"use what a service would use" 

	^RowanAnsweringService new jadeServer
]

{ #category : 'tests' }
JadeServerTest >> test_jadeServerCanonicalLocation [
	"We reuse the topaz session state slot. Safe because this is not a topaz session"
	
	self assert: ((System __sessionStateAt: 3) isKindOf: JadeServer64bit32).
	self assert: ((System __sessionStateAt: 3) class canUnderstand: #updateFromSton:)
]

{ #category : 'tests' }
JadeServerTest >> test_jadeServerExists [

	"at minimum (for now), these classes should always be present:
	Object
		JadeServer
			JadeServer64bit
				JadeServer64bit24
					JadeServer64bit3x
						JadeServer64bit32.

	See RsGsPlatform>>jadeServerClassNamed: for symbol list JadeServer*
	lives in. Note that method will only lookup JadeServer & JadeServer64bit32"
				
	| jadeServerClass |
	#(#JadeServer #JadeServer64bit #JadeServer64bit24 #JadeServer64bit3x #JadeServer64bit32) do:[:className |
		jadeServerClass := UserGlobals at: className. 
		self assert: jadeServerClass name = className asSymbol].
]

{ #category : 'tests' }
JadeServerTest >> test_jadeServerHierarchyValid [

	"The hierarchy should be:
	Object
		JadeServer
			JadeServer64bit
				JadeServer64bit24
					JadeServer64bit3x
						JadeServer64bit32.

	See RsGsPlatform>>jadeServerClassNamed: for symbol list JadeServer*
	lives in. Note that method will only lookup JadeServer & JadeServer64bit32"
				
	| jadeServerClass subclasses |
	jadeServerClass := Rowan platform jadeServerClassNamed: #JadeServer. 
	subclasses :=  jadeServerClass subclasses asArray.
	self assert: subclasses size equals: 1. 
	self assert: subclasses first name = #JadeServer64bit.
	jadeServerClass := UserGlobals at: subclasses first name. 
	subclasses :=  jadeServerClass subclasses asArray.
	self assert: subclasses size equals: 1. 
	self assert: subclasses asArray first name = #JadeServer64bit24.
	jadeServerClass := UserGlobals at: subclasses first name. 
	subclasses :=  jadeServerClass subclasses asArray.
	self assert: subclasses size equals: 1. 
	self assert: subclasses asArray first name = #JadeServer64bit3x.
	jadeServerClass := UserGlobals at: subclasses first name. 
	subclasses :=  jadeServerClass subclasses asArray.
	self assert: subclasses size equals: 1. 
	self assert: subclasses asArray first name = #JadeServer64bit32.
	jadeServerClass := Rowan platform jadeServerClassNamed: #JadeServer64bit32. 
	subclasses :=  jadeServerClass subclasses asArray.
	self assert: subclasses isEmpty
]

{ #category : 'tests' }
JadeServerTest >> test_serviceUsesCanonicalJadeServer [
	
	self assert: (RowanAnsweringService new jadeServer isKindOf: JadeServer64bit32).
	self assert: RowanAnsweringService new jadeServer == (System __sessionStateAt: 3)
]

{ #category : 'tests' }
JadeServerTest >> test_updateFromSton [
	
	"the entry point for all services call is JadeServer>>updateFromSton:.
	Make sure the method is present and will accept/return ston string"

	| service stonString resultString services |

	self assert: (JadeServer canUnderstand: #updateFromSton:).
	service := RowanQueryService new 
				command: #implementorsOf:; 
				commandArgs: (Array with: #test_updateFromSton).
	stonString := STON toString: (Array with: service).
	resultString := self jadeServer updateFromSton: stonString. 
	[services := STON fromString: resultString.
	self assert: services size equals: 1.
	self assert: (services first isKindOf: RowanQueryService).
	self assert: (services first queryResults first isKindOf: RowanMethodService).
	self assert: services first queryResults first selector == #test_updateFromSton]
		ensure: [RowanCommandResult initializeResults.]
]
