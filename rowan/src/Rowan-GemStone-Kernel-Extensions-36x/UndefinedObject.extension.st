Extension { #name : 'UndefinedObject' }

{ #category : '*rowan-gemstone-kernel-extensions-36x' }
UndefinedObject >> subclass: aString 
instVarNames: anArrayOfInstvarNames 
classVars: anArrayOfClassVars 
classInstVars: anArrayOfClassInstVars 
poolDictionaries: anArrayOfPoolDicts 
inDictionary: aDictionary 
newVersionOf: oldClass 
description: aDescription 
constraints: constraintsArray
options: optionsArray [

  | descr theClass superClassOffset |
  descr := aDescription.
  oldClass ifNotNil: [ 
     (Object _equivalentSubclass: oldClass
        superCls: self
        name: aString
        newOpts: optionsArray
        newFormat: oldClass format
        newInstVars: anArrayOfInstvarNames
        newClassInstVars: anArrayOfClassInstVars
        newPools: anArrayOfPoolDicts
        newClassVars: anArrayOfClassVars
        inDict: aDictionary
        constraints: constraintsArray isKernel: false ) ifTrue: [
            oldClass _commentOrDescription: aDescription.
            ^oldClass "avoid creation of a new version"
        ].
      descr ifNil: [descr := oldClass commentForFileout]
  ].
  theClass := Object
        subclass: aString
        instVarNames: anArrayOfInstvarNames
        classVars: anArrayOfClassVars
        classInstVars: anArrayOfClassInstVars
        poolDictionaries: anArrayOfPoolDicts
        inDictionary: aDictionary
        newVersionOf: oldClass
        description: descr
		constraints: constraintsArray
        options: optionsArray.
  theClass == oldClass
    ifFalse:
      [superClassOffset := Behavior _ivOffsetOf: #superClass.
      theClass _unsafeAt: superClassOffset put: nil.
      theClass class _unsafeAt: superClassOffset put: Object class superClass].
  ^theClass

]
