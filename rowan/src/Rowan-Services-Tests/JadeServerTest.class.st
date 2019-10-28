Class {
	#name : 'JadeServerTest',
	#superclass : 'TestCase',
	#category : 'Rowan-Services-Tests'
}

{ #category : 'support' }
JadeServerTest >> jadeiteServer [

	^JadeServer theJadeiteServer
]

{ #category : 'tests' }
JadeServerTest >> test_jadeServerCanonicalLocation [
  | jadeServerClass |
  (System _gemVersion beginsWith: '3.2')
    ifTrue: [ jadeServerClass := JadeServer64bit32 ]
    ifFalse: [ jadeServerClass := JadeServer64bit35 ].
  self assert: (JadeServer theJadeiteServer isKindOf: jadeServerClass).
  self
    assert:
      (JadeServer theJadeiteServer class canUnderstand: #'updateFromSton:').
  self
    assert:
      JadeServer theJadeiteServer == (SessionTemps current at: #'jadeiteServer')
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

	"if method compiles we're in like flint"
	{JadeServer .
			JadeServer64bit .
				JadeServer64bit24 .
					JadeServer64bit3x .
						JadeServer64bit32}.
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
	lives in. Note that method will only lookup JadeServer, JadeServer64bit32,
	and JadeServer64bit35"
				
	| jadeServerClass subclasses |
	jadeServerClass := Rowan platform jadeServerClassNamed: #JadeServer. 
	subclasses :=  jadeServerClass subclasses asArray.
	self assert: subclasses size equals: 1. 
	self assert: subclasses first name = #JadeServer64bit.
	jadeServerClass := Rowan globalNamed: subclasses first name. 
	subclasses :=  jadeServerClass subclasses asArray.
	self assert: subclasses size equals: 1. 
	self assert: subclasses asArray first name = #JadeServer64bit24.
	jadeServerClass :=  Rowan globalNamed: subclasses first name. 
	subclasses :=  jadeServerClass subclasses asArray.
	self assert: subclasses size equals: 1. 
	self assert: subclasses asArray first name = #JadeServer64bit3x.
	jadeServerClass :=  Rowan globalNamed: subclasses first name. 
	subclasses :=  jadeServerClass subclasses asArray.
	self assert: subclasses size equals: 1. 
	self assert: subclasses asArray first name = #JadeServer64bit32.
	jadeServerClass := Rowan platform jadeServerClassNamed: #JadeServer64bit32. 
	subclasses :=  jadeServerClass subclasses asArray.
	self assert: subclasses asArray first name = #JadeServer64bit35.
	jadeServerClass := Rowan platform jadeServerClassNamed: #JadeServer64bit35. 
	subclasses :=  jadeServerClass subclasses asArray.
	self assert: subclasses isEmpty
]

{ #category : 'tests' }
JadeServerTest >> test_rowanCanFindJadeServer [

	self assert: (Rowan class canUnderstand: #jadeServerClassNamed:).
	self assert: (RwGsPlatform canUnderstand: #jadeServerClassNamed:).
	self assert: (Rowan jadeServerClassNamed: #JadeServer64bit32) equals: JadeServer64bit32.
	self assert: (Rowan jadeServerClassNamed: #JadeServer64bit35)equals: JadeServer64bit35
]

{ #category : 'tests' }
JadeServerTest >> test_serviceUsesCanonicalJadeServer [
  | jadeServerClass |
  (System _gemVersion beginsWith: '3.2')
    ifTrue: [ jadeServerClass := JadeServer64bit32 ]
    ifFalse: [ jadeServerClass := JadeServer64bit35 ].
  self
    assert: (RowanAnsweringService new jadeiteServer isKindOf: jadeServerClass).
  self
    assert:
      RowanAnsweringService new jadeiteServer == JadeServer theJadeiteServer
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
	resultString := self jadeiteServer updateFromSton: stonString. 
	[services := STON fromString: resultString.
	self assert: services size equals: 3.
	self assert: (services first isKindOf: RowanMethodService).
	self assert: ((services at: 2) isKindOf: RowanQueryService).
	self assert: ((services at: 2) queryResults first isKindOf: RowanMethodService).
	self assert: (services at: 2) queryResults first selector == #test_updateFromSton.
	self assert: (services last isKindOf: RowanLoggingService)]
		ensure: [RowanCommandResult initializeResults.]
]
