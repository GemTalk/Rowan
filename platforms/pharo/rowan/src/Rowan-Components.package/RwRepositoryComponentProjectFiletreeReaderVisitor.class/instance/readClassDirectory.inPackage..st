class reading
readClassDirectory: classDirectory inPackage: packageName

	| classDirectoryReference classProperties classComment normalizedMetadata classDef |
	classDirectoryReference := classDirectory asFileReference.
	classProperties :=  self _readObjectFrom: classDirectoryReference / 'properties' , 'json'.
	classComment := (classDirectoryReference / 'README' , 'md') contents.
	normalizedMetadata := Dictionary new.
	classProperties keysAndValuesDo: [:key :value |
		normalizedMetadata at: key asLowercase asSymbol put: value ].
	classDef :=	self newClassDefinitionFrom: {
		nil.
		classComment. 
		nil. 
		normalizedMetadata at: 'type' ifAbsent: ['normal']. 
		nil. 
		normalizedMetadata
	}.
	self validateClassCategory: classDef forPackageNamed: packageName.
	(currentProjectDefinition packageNamed: packageName)
		addClassDefinition: currentClassDefinition.

	self readMethodDirectories: classDirectoryReference forClassDefinition: classDef inPackage: packageName