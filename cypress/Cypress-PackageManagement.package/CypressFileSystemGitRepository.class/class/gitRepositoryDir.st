"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: accessing
"
gitRepositoryDir
  | path |
  path := (SessionTemps current at: #'Cypress_FileSystem_Git_Repository_Directory' otherwise: nil)
    ifNil: [
      path := (System gemEnvironmentVariable: 'GEMSTONE_GITDIR')
        ifNil: [ 
          CypressFileUtilities current workingDirectory
          , CypressFileUtilities current pathNameDelimiter , 'cypress-git-repos' ].
      SessionTemps current at: #'Cypress_FileSystem_Git_Repository_Directory' put: path ].
  ^path
