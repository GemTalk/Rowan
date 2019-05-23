method reading
readMethodDirectory: methodDirectory forClassDefinition: classDefinition isClassMeta: isClassMeta inPackage: packageName

	| methodDefinitions methodDefinitionStream |
	methodDefinitions := {}.
	methodDirectory exists ifFalse: [ ^ methodDefinitions ].

	methodDefinitionStream := methodDefinitions writeStreamPortable.
	methodDirectory files do: [:file |
		file extension = 'st'
			ifTrue: [ 
				| protocol methodSource methodStream methodDef |
				methodStream := file contents  readStreamPortable.
				protocol := methodStream nextLine.
				methodSource := methodStream upToEnd.
				methodDef := RwMethodDefinition newForSource: methodSource protocol: protocol.
				self 
					validateMethodDefinitionProtocol: methodDef 
						className: classDefinition name
						isMeta: isClassMeta 
						forPackageNamed:  packageName.
				methodDefinitionStream nextPut: methodDef ] ].

	^ methodDefinitions