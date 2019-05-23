support - singleClassVersionChange definitions
singleClassVersionChangeClassDefinitions2: packageName className: className

	"Single class with method foo and new class version."

	"add iva and preserve foo unchanged. "

	^ Dictionary new
		at: className
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: className
								super: 'Object'
								instvars: #('iva')
								classinstvars: #()
								classvars: #()
								comment: 'a comment'
								pools: {}
								inPackage: packageName)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions:
							(self methodDefsFromSpec: {#(#'foo' 'accessing' 'foo ^''foo''')}));
		yourself