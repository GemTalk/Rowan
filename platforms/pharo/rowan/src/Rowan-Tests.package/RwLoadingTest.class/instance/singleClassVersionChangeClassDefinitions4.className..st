support - singleClassVersionChange definitions
singleClassVersionChangeClassDefinitions4: packageName className: className

	"Single class with method foo and new class version."

	"add civa and change protocol of foo. "

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
								classvars: #('CVA')
								comment: 'a comment'
								pools: {}
								inPackage: packageName)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions:
							(self methodDefsFromSpec: {#(#'foo' 'fooing' 'foo ^''foo''')}));
		yourself