method reading
readMethodDirectories: classDirectory forClassDefinition: classDefinition inPackage: packageName

	(self readMethodDirectory: classDirectory / 'instance' forClassDefinition: classDefinition isClassMeta: false inPackage: packageName) do: [:methodDefinition |
		classDefinition addInstanceMethodDefinition: methodDefinition ].
	(self readMethodDirectory: classDirectory / 'class' forClassDefinition: classDefinition isClassMeta: true inPackage: packageName) do: [:methodDefinition |
		classDefinition addClassMethodDefinition: methodDefinition ]