method writing
_createFileNameMapForMethods: aMethodDefinitionDictionary
  "https://github.com/dalehenrich/filetree/issues/92"

  "answer a dictionary that maps each definition selector to a filename that is guaranteed unique on case insensitive file systems"

  | map filenameMap |
  map := Dictionary new.
  aMethodDefinitionDictionary
    do: [ :mDef | 
      | sel col |
      "sort into bins by lowercase selector. "
      sel := mDef selector asLowercase.
      col := map
        at: sel
        ifAbsentPut: [ OrderedCollection new ].
      col add: mDef ].
  filenameMap := Dictionary new.
  map values
	do: [ :col | 
	  | sortedCol |
	  col size = 1
		ifTrue: [ 
		  | def |
		  "no need to distinguish filename"
		  def := col at: 1.
		  filenameMap
			at: def selector
			put: (self _methodFileNameFor: def) ]
		ifFalse: [ 
		  "tack on postfix to guarantee  file names are unique on case insensitive file systems"
		  sortedCol := col sorted: [ :a :b | a asString _unicodeLessThan: b asString ].
		  (1 to: sortedCol size) do: [ :index | 
			| def filename |
			def := sortedCol at: index.
			filename := self _methodFileNameFor: def.
			filename := filename , '..' , index printString.
			filenameMap at: def selector put: filename ] ] ].
  ^ filenameMap