Extension { #name : 'Behavior' }

{ #category : '*rowan-gemstone-35x' }
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

{ #category : '*rowan-gemstone-35x' }
Behavior >> _constraintOn: aSymbol [

"Returns the class kind constraint for the instance variable represented by
 aSymbol.  If aSymbol does not represent an instance variable of objects whose
 behavior is defined by the receiver, returns nil.
 If the instance variable aSymbol is not constrained, returns Object ."

| ivNams constrs |

ivNams := instVarNames .
constrs := constraints .
1 to: self instSize do: [ :i |
  aSymbol == (ivNams  at: i) ifTrue:[ ^ self _constraintAt: i ].
].
^ nil
]

{ #category : '*rowan-gemstone-35x' }
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

{ #category : '*rowan-gemstone-35x' }
Behavior >> _namedIvConstraintAtOffset: offset [

"Returns the constraint, if any, on the named instance variable at the
 specified offset.  Returns Object if there is no such named instance variable,
 or if the instance variable at that offset is not constrained."

(offset > self instSize ) ifTrue:[ ^ Object ] .
^ self _constraintAt: offset 
]

{ #category : '*rowan-gemstone-35x' }
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

{ #category : '*rowan-gemstone-35x' }
Behavior >> _setConstraintBit [

"Sets the constraint bit in the 'format' instance variable of the receiver."

self deprecated: 'Behavior>>_setConstraintBit deprecated, Constraints are no longer supported'.
self _validatePrivilege ifTrue:[
  format := format bitOr: 16#10 .
]
]
