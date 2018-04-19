Extension { #name : 'Collection' }

{ #category : '*tonel-gemstonecommon-core' }
Collection >> asDictionary [

  | dict |
  dict := Dictionary new.
  self do: [:assoc |
    dict add: assoc].
  ^ dict

]

{ #category : '*tonel-gemstonecommon-core' }
Collection >> flattened [
	
	"Flattens a collection of collections (no matter how many levels of collections exist).
	Strings are considered atoms and, as such, won't be flattened
	
	Examples:
	#(1 #(2 3) #(4 (#5))) flattened returns #(1 2 3 4 5) 
	#('string1' #('string2' 'string3')) flattened returns #('string1' 'string2' 'string3')"
	
	^ Array streamContents: [ :stream | self flattenOn: stream].

]

{ #category : '*tonel-gemstonecommon-core' }
Collection >> flattenOn: aStream [

	self do: [ :each | (each isCollection and: [each isString not]) 
						ifTrue: [each flattenOn: aStream]
						ifFalse: [aStream nextPut: each]].

]
