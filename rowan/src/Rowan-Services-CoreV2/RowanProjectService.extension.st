Extension { #name : 'RowanProjectService' }

{ #category : '*rowan-services-corev2' }
RowanProjectService >> addPackageNamed: packageName toComponentNamed: componentName [

	Rowan image loadedPackageNamed: packageName ifAbsent: [
		self browserTool addPackageNamed: packageName toComponentNamed: componentName andProjectNamed: name. 
		self update.
		^self answer: #added.].
	self answer: #duplicatePackage
]
