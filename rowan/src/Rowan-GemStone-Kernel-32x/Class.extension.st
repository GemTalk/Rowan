Extension { #name : 'Class' }

{ #category : '*rowan-gemstone-kernel-32x' }
Class >> _rwOptionsArray [
  "copy of _optionsArray"

  | result optCount | 
  result := { } .
  optCount := 0 .
  self instancesDbTransient ifTrue:[ result add: #dbTransient . optCount := optCount + 1 ].
  self instancesNonPersistent ifTrue:[ result add:  #instancesNonPersistent  . optCount := optCount + 1 ].
  self instancesInvariant ifTrue:[ result add:  #instancesInvariant  . optCount := optCount + 1 ].
  optCount > 1 ifTrue:[
    self _error: #classErrBadFormat
        with:'only one of #dbTransient #instancesNonPersistent  #instancesInvariant allowed' .
  ].
  "self _structuralUpdatesDisallowed ifTrue:[ result add: #disallowGciStore  ]." "commented out variant of _optionsArray (https://github.com/dalehenrich/Rowan/issues/292)"
  self isModifiable ifTrue:[ result add: #modifiable  ].
  self subclassesDisallowed ifTrue:[ result add: #subclassesDisallowed  ].
  "self _traversalByCallback ifTrue:[ result add: #traverseByCallback  ]." "commented out variant of _optionsArray (https://github.com/dalehenrich/Rowan/issues/292)"
  ^ result
]