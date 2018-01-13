Class {
	#name : 'RwGsPatchSetAbstract',
	#superclass : 'Object',
	#category : 'Rowan-GemStone'
}

{ #category : 'building' }
RwGsPatchSetAbstract >> addAddedClass: aClassDefinition inPackage: aPackageDefinition [

	self subclassResponsibility: #'addAddedClass:inPackage:'
]

{ #category : 'building' }
RwGsPatchSetAbstract >> addAddedClassMethod: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition [

	self subclassResponsibility: #'addAddedClassMethod:inClass:inPackage:'
]

{ #category : 'building' }
RwGsPatchSetAbstract >> addAddedInstanceMethod: anInstanceMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition [

	self subclassResponsibility: #'addAddedInstanceMethod:inClass:inPackage:'
]

{ #category : 'building' }
RwGsPatchSetAbstract >> addAddedPackage: aPackageDefinition [

	self subclassResponsibility: #'addAddedPackage:'
]

{ #category : 'building' }
RwGsPatchSetAbstract >> addDeletedClass: aClassDefinition inPackage: aPackageDefinition [

	self subclassResponsibility: #'addDeletedClass:inPackage:'
]

{ #category : 'building' }
RwGsPatchSetAbstract >> addDeletedClassMethod: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition [

	self subclassResponsibility: #'addDeletedClassMethod:inClass:inPackage:'
]

{ #category : 'building' }
RwGsPatchSetAbstract >> addDeletedInstanceMethod: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition [

	self subclassResponsibility: #'addDeletedInstanceMethod:inClass:inPackage:'
]

{ #category : 'building' }
RwGsPatchSetAbstract >> addDeletedPackage: aPackageDefinition [

	self subclassResponsibility: #'addDeletedPackage:'
]

{ #category : 'building' }
RwGsPatchSetAbstract >> addPatchedClassClassVariables: aClassDefinition inPackage: aPackageDefinition [

	self subclassResponsibility: #'addPatchedClassClassVariables:inPackage:'
]

{ #category : 'building' }
RwGsPatchSetAbstract >> addPatchedClassMethod: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition [

	self subclassResponsibility: #'addPatchedClassMethod:inClass:inPackage:'
]

{ #category : 'building' }
RwGsPatchSetAbstract >> addPatchedClassMethodProperties: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition [

	self
		subclassResponsibility: #'addPatchedClassMethodProperties:inClass:inPackage:'
]

{ #category : 'building' }
RwGsPatchSetAbstract >> addPatchedClassNewVersion: aClassDefinition inPackage: aPackageDefinition [

	self subclassResponsibility: #'addPatchedClassNewVersion:inPackage:'
]

{ #category : 'building' }
RwGsPatchSetAbstract >> addPatchedClassProperties: aClassDefinition inPackage: aPackageDefinition [

	self subclassResponsibility: #'addPatchedClassProperties:inPackage:'
]

{ #category : 'building' }
RwGsPatchSetAbstract >> addPatchedInstanceMethod: anInstanceMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition [

	self subclassResponsibility: #'addPatchedInstanceMethod:inClass:inPackage:'
]

{ #category : 'building' }
RwGsPatchSetAbstract >> addPatchedInstanceMethodProperties: anInstanceMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition [

	self
		subclassResponsibility:
			#'addPatchedInstanceMethodProperties:inClass:inPackage:'
]
