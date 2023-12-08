Extension { #name : 'Collection' }

{ #category : '*rowan-tonel-gemstone-kernel' }
Collection >> _flattendAppend: anArray [

	self do: [ :each | (each isCollection and: [each isString not]) 
						ifTrue: [each _flattendAppend: anArray]
						ifFalse: [anArray add: each]].

]

{ #category : '*rowan-tonel-gemstone-kernel' }
Collection >> asDictionary [

  | dict |
  dict := Dictionary new.
  self do: [:assoc |
    dict add: assoc].
  ^ dict

]

{ #category : '*rowan-tonel-gemstone-kernel' }
Collection >> flattened [
	
	"Flattens a collection of collections (no matter how many levels of collections exist).
	Strings are considered atoms and, as such, won't be flattened
	
	Examples:
	#(1 #(2 3) #(4 (#5))) flattened returns #(1 2 3 4 5) 
	#('string1' #('string2' 'string3')) flattened returns #('string1' 'string2' 'string3')"
	
	"^ Array streamContents: [ :stream | self flattenOn: stream]."
  | a |
  a := Array new .
  self _flattendAppend: a .
  ^ a
]

{ #category : '*rowan-tonel-gemstone-kernel' }
Collection >> flattenOn: aStream [

	self do: [ :each | (each isCollection and: [each isString not]) 
						ifTrue: [each flattenOn: aStream]
						ifFalse: [aStream nextPut: each]].

]

{ #category : '*rowan-tonel-gemstone-kernel' }
Collection >> ifNotEmpty: aBlock [

	^ self size == 0
		ifFalse: [ aBlock cull: self ]

]

{ #category : '*rowan-tonel-gemstone-kernel' }
Collection >> isCollection [

	"Return true if the receiver is some sort of Collection and responds to basic collection messages such as #size and #do:"

	^ true

]

{ #category : '*rowan-tonel-gemstone-kernel' }
Collection >> select: selectBlock thenDo: doBlock [
  "Utility method to improve readability."

  ^ (self select: selectBlock) do: doBlock

]

{ #category : '*rowan-tonel-gemstone-kernel' }
Collection >> sort [

	"Sort this array into ascending order using the '<=' operator."

	^ self sort: [ :a :b | a <= b ]

]

{ #category : '*rowan-tonel-gemstone-kernel' }
Collection >> sort: aSortBlock [

	"Sort this array using aSortBlock. The block should take two arguments
	and return true if the first element should preceed the second one."

	^ self sortWithBlock: aSortBlock

]
