"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: writing
"
removeOldReplacingWithNew
  self fileUtils
    deleteAll: self packageDirectory
    rejecting: [ :filename | 
      "do not delete the monticello.meta directory to preserve existing Monticello meta data.
       Equivalent behavior to MCFileTreeRepository with Metadata property set to false."
      filename endsWith: 'monticello.meta' ].
  self writePropertiesFile.
  self writePackageStructure