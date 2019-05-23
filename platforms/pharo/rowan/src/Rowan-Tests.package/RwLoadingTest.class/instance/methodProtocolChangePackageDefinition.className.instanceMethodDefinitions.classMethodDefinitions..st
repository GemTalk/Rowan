support - methodProtocolChange definitions
methodProtocolChangePackageDefinition: packageName className: className instanceMethodDefinitions: instanceMethodDefinitions classMethodDefinitions: classMethodDefinitions
  ^ self
    methodDeletionPackageDefinition: packageName
    className: className
    instanceMethodDefinitions: instanceMethodDefinitions
    classMethodDefinitions: classMethodDefinitions