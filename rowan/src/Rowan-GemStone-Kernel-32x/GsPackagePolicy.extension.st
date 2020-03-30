Extension { #name : 'GsPackagePolicy' }

{ #category : '*rowan-gemstone-kernel-32x' }
GsPackagePolicy class >> currentOrNil [

	"Returns nil or the previously installed and enabled GsPackagePolicy."

	| pp |
	pp := ((GsSession currentSession objectNamed: #'UserGlobals') ifNil: [ ^ nil ])
		at: self globalName
		otherwise: nil.
	pp
		ifNotNil: [ 
			pp enabled
				ifTrue: [ ^ pp ] ].
	^ nil

]
