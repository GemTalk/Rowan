"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: writing
"
writeExtensionClassStructure: classStructure to: classPath
  self
    writeInDirectoryName: classPath
    fileName: 'properties'
    extension: self propertiesFileNameExtension
    visit: [ :fileStream | 
      (Dictionary with: 'name' -> classStructure className)
        writeFiletreeJsonOn: fileStream ]