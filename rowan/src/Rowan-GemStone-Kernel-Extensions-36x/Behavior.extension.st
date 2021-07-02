Extension { #name : 'Behavior' }

{ #category : '*rowan-gemstone-kernel-extensions-36x' }
Behavior >> _ivOffsetAndConstraint: aSymbol [

"Searches the instVarNames instance variable of the receiver for an instance
 variable named aSymbol, and returns an Array containing the offset and the
 constraint for that instance variable.  Returns nil if no instance variable
 exists with the name aSymbol."

| idx |
idx := instVarNames indexOfIdentical: aSymbol .
idx == 0 ifTrue:[ ^ nil ].
^ { idx .  self _constraintAt: idx } 

]

{ #category : '*rowan-gemstone-kernel-extensions-36x' }
Behavior >> _namedIvConstraintAtOffset: offset [

"Returns the constraint, if any, on the named instance variable at the
 specified offset.  Returns Object if there is no such named instance variable,
 or if the instance variable at that offset is not constrained."

(offset > self instSize ) ifTrue:[ ^ Object ] .
^ self _constraintAt: offset 

]

{ #category : '*rowan-gemstone-kernel-extensions-36x' }
Behavior >> _newConstraint: aClass atOffset: offset [

"Execute the constraint change for Behavior | instvar:ConstraintTo:
 assuming all error and variance checks have been done."
| constrs |
self deprecated: 'Behavior>>_newConstraint:atOffset: deprecated, Constraints are no longer supported'.
self _validatePrivilege ifTrue:[ 
  (constrs := constraints) size == 0 ifTrue:[ | sz |
    aClass == Object ifTrue:[ ^ self "do nothing"].
    sz := self instSize .
    (constrs := Array new: sz ) replaceFrom: 1 to: sz withObject: Object.
    constraints := constrs .
  ].
  constrs at: offset put: aClass .
  (aClass == Object) ifFalse:[ self _setConstraintBit ].
  self _refreshClassCache: false .
]

]

{ #category : '*rowan-gemstone-kernel-extensions-36x' }
Behavior >> _setConstraintBit [

"Sets the constraint bit in the 'format' instance variable of the receiver."

self deprecated: 'Behavior>>_setConstraintBit deprecated, Constraints are no longer supported'.
self _validatePrivilege ifTrue:[
  format := format bitOr: 16#10 .
]

]

{ #category : '*rowan-gemstone-kernel-extensions-36x' }
Behavior >> _setVaryingConstraint: aClass [

"Assign a new value to the constraint on unnamed variables of the receiver,
 assuming all checks have been made."

| constrs ofs |

self deprecated: 'Behavior>>_setVaryingConstraint: deprecated, Constraints are no longer supported'.
self _validatePrivilege ifTrue:[
  constrs := constraints .
  ofs := self instSize + 1 .
  constrs size == 0 ifTrue:[ 
    aClass == Object ifTrue:[ ^ self "nothing to do"].
    (constrs := Array new: ofs) replaceFrom: 1 to: ofs withObject: Object .
    constraints := constrs .
  ].
  constrs at: ofs put: aClass .
  (aClass == Object) ifFalse:[ self _setConstraintBit ].
  self _refreshClassCache: false .
]

]

{ #category : '*rowan-gemstone-kernel-extensions-36x' }
Behavior >> rwGuaranteePersistentMethodDictForEnv: envId [
	"in 3.5, the method persistentMethodDictForEnv: DOES NOT always return a GsMethodDictionary,
		as classes are created without a GsMethodDictionary for envId 0."

	<primitive: 2001>
	| prot |
	prot := System _protectedMode .
	[ 
		| newDict |
		(self persistentMethodDictForEnv: envId) ifNotNil: [:oldDict | ^ oldDict ].
		newDict := GsMethodDictionary new.
		self persistentMethodDictForEnv: envId put: newDict.
		^ newDict ] 
		ensure:[ prot _leaveProtectedMode ].

]
