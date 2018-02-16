Class {
	#name : 'RwGsPatchSetAbstract',
	#superclass : 'Object',
	#category : 'Rowan-GemStone'
}

{ #category : 'building' }
RwGsPatchSetAbstract >> addAddedClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

	self subclassResponsibility: #'addAddedClass:inPackage:inProject:'
]

{ #category : 'building' }
RwGsPatchSetAbstract >> addAddedClassMethod: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

	self
		subclassResponsibility: #'addAddedClassMethod:inClass:inPackage:inProject:'
]

{ #category : 'building' }
RwGsPatchSetAbstract >> addAddedInstanceMethod: anInstanceMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

	self
		subclassResponsibility: #'addAddedInstanceMethod:inClass:inPackage:inProject:'
]

{ #category : 'building' }
RwGsPatchSetAbstract >> addAddedPackage: aPackageDefinition inProject: aProjectDefinition [

	self subclassResponsibility: #'addAddedPackage:inProject:'
]

{ #category : 'building' }
RwGsPatchSetAbstract >> addAddedProject: aProjectDefinition [

	self subclassResponsibility: #'addAddedProject:'
]

{ #category : 'building' }
RwGsPatchSetAbstract >> addDeletedClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

	self subclassResponsibility: #'addDeletedClass:inPackage:inProject:'
]

{ #category : 'building' }
RwGsPatchSetAbstract >> addDeletedClassMethod: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

	self
		subclassResponsibility: #'addDeletedClassMethod:inClass:inPackage:inProject:'
]

{ #category : 'building' }
RwGsPatchSetAbstract >> addDeletedInstanceMethod: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

	self
		subclassResponsibility:
			#'addDeletedInstanceMethod:inClass:inPackage:inProject:'
]

{ #category : 'building' }
RwGsPatchSetAbstract >> addDeletedPackage: aPackageDefinition inProject: aProjectDefinition [

	self subclassResponsibility: #'addDeletedPackage:inProject:'
]

{ #category : 'building' }
RwGsPatchSetAbstract >> addExtendedClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

	self subclassResponsibility: #'addExtendedClass:inPackage:inProject:'
]

{ #category : 'building' }
RwGsPatchSetAbstract >> addExtendedClassMethod: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

	self
		subclassResponsibility: #'addExtendedClassMethod:inClass:inPackage:inProject:'
]

{ #category : 'building' }
RwGsPatchSetAbstract >> addExtendedInstanceMethod: anInstanceMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

	self
		subclassResponsibility:
			#'addExtendedInstanceMethod:inClass:inPackage:inProject:'
]

{ #category : 'building' }
RwGsPatchSetAbstract >> addPatchedClassClassVariables: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

	self
		subclassResponsibility: #'addPatchedClassClassVariables:inPackage:inProject:'
]

{ #category : 'building' }
RwGsPatchSetAbstract >> addPatchedClassMethod: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

	self
		subclassResponsibility: #'addPatchedClassMethod:inClass:inPackage:inProject:'
]

{ #category : 'building' }
RwGsPatchSetAbstract >> addPatchedClassMethodProperties: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

	self
		subclassResponsibility:
			#'addPatchedClassMethodProperties:inClass:inPackage:inProject:'
]

{ #category : 'building' }
RwGsPatchSetAbstract >> addPatchedClassNewVersion: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

	self subclassResponsibility: #'addPatchedClassNewVersion:inPackage:inProject:'
]

{ #category : 'building' }
RwGsPatchSetAbstract >> addPatchedClassProperties: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

	self subclassResponsibility: #'addPatchedClassProperties:inPackage:inProject:'
]

{ #category : 'building' }
RwGsPatchSetAbstract >> addPatchedInstanceMethod: anInstanceMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

	self
		subclassResponsibility:
			#'addPatchedInstanceMethod:inClass:inPackage:inProject:'
]

{ #category : 'building' }
RwGsPatchSetAbstract >> addPatchedInstanceMethodProperties: anInstanceMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition inProject: aProjectDefinition [

	self
		subclassResponsibility:
			#'addPatchedInstanceMethodProperties:inClass:inPackage:inProject:'
]
