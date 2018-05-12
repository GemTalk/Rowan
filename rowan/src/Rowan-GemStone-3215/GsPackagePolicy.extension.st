Extension { #name : 'GsPackagePolicy' }

{ #category : '*rowan-gemstone-3215' }
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
