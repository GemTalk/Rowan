support - singleClassVersionChange definitions
singleClassVersionChangeClassDefinitions3: packageName className: className

	"Single class with method foo and new class version."

	"add civa and change foo. "

	^ Dictionary new
		at: className
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: className
								super: 'Object'
								instvars: #('iva')
								classinstvars: #('civa')
								classvars: #()
								comment: 'a comment'
								pools: {}
								inPackage: packageName)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions:
							(self methodDefsFromSpec: {#(#'foo' 'accessing' 'foo ^''bar''')}));
		yourself