tests
testClassCreationTemplate

	| template x |
	template := Rowan projectTools browser
		classCreationTemplateForSubclassOf: 'Object'
		category: 'MyClasses'.
	self
		assert: template = self _expectedHybridClassCreationTemplate
		description: 'incorrect class creation template'.

	template := Rowan projectTools browser
		classCreationTemplateForSubclassOf: 'Object'
		category: 'MyClasses'
		packageName: 'MyPackage'.
	self
		assert: template = self _expectedClassCreationTemplate
		description: 'incorrect class creation template'.

	template := Rowan projectTools browser
		classCreationTemplateForClass: ByteArray
		hybridBrowser: true.
	self
		assert: template = (x := self _expectedHybridByteArrayCreationTemplate)
		description: 'incorrect class creation template'.

	template := Rowan projectTools browser
		classCreationTemplateForClass: ByteArray
		hybridBrowser: false.
	self
		assert: template = (x := self _expectedHybridByteArrayCreationTemplate)
		description: 'incorrect class creation template'.

	template := Rowan projectTools browser
		classCreationTemplateForClass: Collection
		hybridBrowser: false.
	self
		assert: template = self _expectedCollectionCreationTemplate
		description: 'incorrect class creation template'.

	template := Rowan projectTools browser
		classCreationTemplateForClass: Association
		hybridBrowser: false.
	self
		assert: template = (x := self _expectedAssociationCreationTemplate)
		description: 'incorrect class creation template'