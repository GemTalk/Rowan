Extension { #name : 'RowanProjectService' }

{ #category : '*rowan-services-corev1' }
RowanProjectService >> addPackageNamed: packageName [

	Rowan image loadedPackageNamed: packageName ifAbsent: [
		self browserTool addPackageNamed: packageName toProjectNamed: name. 
		self update.
		^self answer: #added.].
	self answer: #duplicatePackage
]
