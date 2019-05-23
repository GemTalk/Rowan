private
_createFileNameMapForClassesOrPackages: aClassOrPackageDefinitionDictionary
 
  "answer a dictionary that maps each definition name to a filename that is guaranteed unique on case insensitive file systems"

  | map filenameMap |
  map := Dictionary new.
  aClassOrPackageDefinitionDictionary
    do: [ :cDef | 
      | nam col |
      "sort into bins by lowercase selector. "
      nam := cDef name asLowercase.
      col := map
        at: nam
        ifAbsentPut: [ OrderedCollection new ].
      col add: cDef ].
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
			at: def name
			put: def name ]
		ifFalse: [ 
		  "tack on postfix to guarantee  file names are unique on case insensitive file systems"
		  sortedCol := col sorted: [ :a :b | a asString _unicodeLessThan: b asString ].
		  (1 to: sortedCol size) do: [ :index | 
			| def filename |
			def := sortedCol at: index.
			filename := def name.
			filename := filename , '..' , index printString.
			filenameMap at: def name put: filename ] ] ].
  ^ filenameMap