support - singleClassVersionChange definitions
singleClassVersionChangeClassDefinitions1: packageName className: className

	"Single class with a method #foo. and new class version"

	^ Dictionary new
		at: className
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: className
								super: 'Object'
								instvars: #()
								classinstvars: #()
								classvars: #()
								comment: 'a comment'
								pools: {}
								inPackage: packageName)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions:
							(self methodDefsFromSpec: {#(#'foo' 'accessing' 'foo ^''foo''')}));
		yourself