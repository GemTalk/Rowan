Class {
	#name : 'JadeServerTest',
	#superclass : 'TestCase',
	#category : 'Rowan-Services-Tests'
}

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
