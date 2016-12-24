"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
moveExtensionMethodsToPackageOfProtocol
	"Where tools do not yet fully understand Cypress packages, but do handle class categories and method protocols,
	assign each class in the system to some LoadedPackage. This will create LoadedPackages as required."

	self allClassesDo: 
			[:aClass |
			self allInstanceMethodsOf: aClass
				do: 
					[:aMethod |
					| protocol |
					protocol := self protocolOfMethod: aMethod.
					(protocol at: 1) = $*
						ifTrue: 
							[| oldPackage newPackage |
							newPackage := Cypress
										existingOrNewLoadedPackageNamed: (protocol copyFrom: 2 to: protocol size).
							oldPackage := nil]]]