Class {
	#name : 'RwGsClassConstraintsSymDictPatch',
	#superclass : 'RwGsClassPatch',
	#category : 'Rowan-GemStone-Loader'
}

{ #category : 'actions' }
RwGsClassConstraintsSymDictPatch >> addToNewClassesByName: aDictionary [

	"noop"

	
]

{ #category : 'installing' }
RwGsClassConstraintsSymDictPatch >> installPropertiesPatchFor: aPatchSet [

	self installPropertiesPatchFor: aPatchSet registry: self symbolDictionaryRegistry

]

{ #category : 'installing' }
RwGsClassConstraintsSymDictPatch >> installPropertiesPatchFor: aPatchSet registry: aSymbolDictionaryRegistry [
	" update class and update loadedClass with new constraints"

	| className existingClass theConstraints existingConstraintsMap existingVaryingConstraint theConstraintsMap theVaryingConstraint keys existingConstraints instVarNames superConstraints superConstraintsMap superC superInstVarNames |
	className := classDefinition key asSymbol.
	existingClass := aPatchSet createdClasses
		at: className
		ifAbsent: [ 
			aPatchSet tempSymbols
				at: className
				ifAbsent: [ self error: 'Cannot find class to update constraints for.' ] ].
	superConstraintsMap := Dictionary new.
	existingConstraintsMap := Dictionary new.
	existingVaryingConstraint := existingClass _varyingConstraint.
	instVarNames := existingClass allInstVarNames.
	superC := existingClass superClass.
	superInstVarNames := superC ifNil: [ {} ] ifNotNil: [ superC allInstVarNames ].
	[ 
	superConstraints := superC
		ifNil: [ {} ]
		ifNotNil: [ :superCl | superCl _constraints ifNil: [ {} ] ].
	existingConstraints := existingClass _constraints ]
		on: Deprecated
		do: [ :ex | ex resume ].
	1 to: superConstraints size do: [ :index | 
		superConstraintsMap
			at: (superInstVarNames at: index)
			put: (superConstraints at: index) ].
	1 to: existingConstraints size do: [ :index | 
		existingConstraintsMap
			at: (instVarNames at: index)
			put: (existingConstraints at: index) ].
	theConstraints := self _gemStoneConstraintsFrom: classDefinition gs_constraints.
	theConstraintsMap := Dictionary new.
	theVaryingConstraint := Object.
	theConstraints
		do: [ :arrayOrVaryingConstraintClass | 
			arrayOrVaryingConstraintClass _isArray
				ifTrue: [ 
					theConstraintsMap
						at: (arrayOrVaryingConstraintClass at: 1)
						put: (arrayOrVaryingConstraintClass at: 2) ]
				ifFalse: [ theVaryingConstraint := arrayOrVaryingConstraintClass ] ].
	keys := existingConstraintsMap keys copy.
	keys addAll: theConstraintsMap keys.
	keys
		do: [ :key | 
			| existingConstraint theConstraint superConstraint instVarString |
			existingConstraint := existingConstraintsMap at: key ifAbsent: [  ].
			superConstraint := superConstraintsMap at: key ifAbsent: [  ].
			theConstraint := theConstraintsMap at: key ifAbsent: [  ].
			instVarString := key asString.
			existingConstraint == theConstraint
				ifTrue: [ 
					(theConstraint isNil and: [ superConstraint notNil ])
						ifTrue: [ 
							"this is not possible -: both existing and new constraint not defined?"
							existingClass _rwInstVar: instVarString constrainTo: superConstraint  ] ]
				ifFalse: [ 
					existingConstraint == nil
						ifTrue: [ 
							"add theConstraint"
							existingClass _rwInstVar: instVarString constrainTo: theConstraint ]
						ifFalse: [ 
							theConstraint == nil
								ifTrue: [ 
									superConstraint notNil
										ifTrue: [ 
											existingConstraint ~= superConstraint
												ifTrue: [ 
													"inherit constraint from superclass"
													existingClass _rwInstVar: instVarString constrainTo: superConstraint ] ]
										ifFalse: [ 
											"remove the constraint"
											existingClass _rwInstVar: instVarString constrainTo: Object ] ]
								ifFalse: [ 
									"change the value of the constraint"
									existingClass _rwInstVar: instVarString constrainTo: theConstraint ] ] ] ].
	existingVaryingConstraint == theVaryingConstraint
		ifFalse: [ 
			"change the varying constraint"
			[ existingClass _setVaryingConstraint: theVaryingConstraint ]
				on: Deprecated
				do: [ :ex | ex resume ] ].
	aSymbolDictionaryRegistry
		updateClassProperties: existingClass
		implementationClass: RwGsSymbolDictionaryRegistry_Implementation
]
