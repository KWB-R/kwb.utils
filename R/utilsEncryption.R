#
# The functions defined in this file need package "PKI"
#

# generateKeyFile --------------------------------------------------------------
generateKeyFile <- function # generate a decryption key file
### generate a decryption key file
(
  target
  ### full path to the file to which the key shall be written
)
{
  PKI.save.key(key = PKI.genRSAkey(), target = target) 
}

# createPasswordFile -----------------------------------------------------------
createPasswordFile <- function # create encrypted password file for account
### create encrypted password file for account
(
  account,
  ### name of account the user is asked to enter the password for
  keyFile,
  ### file containing the encryption/decryption key
  passwordFile
)
{
  password <- .askForPassword(account)
  
  password.encrypted <- PKI.encrypt(
    charToRaw(password), key = PKI.load.key(file = keyFile)
  )
  
  writeBin(password.encrypted, passwordFile)
}

# .askForPassword --------------------------------------------------------------
.askForPassword <- function(account)
{
  clearConsole()
  userInput <- readline(
    paste0("Enter password for account '", account, "': "))
  clearConsole()
  
  userInput
}

# getPassword ------------------------------------------------------------------
getPassword <- function # get encrypted password from file using key
### get encrypted password from file using key
(
  passwordFile, keyFile
) 
{
  if (!file.exists(passwordFile)) {
    stop("Password file '", passwordFile, "' does not exist.")  
  }
  
  password.encrypted <- readBin(passwordFile, what = "raw", n = 256)
  
  rawToChar(
    PKI.decrypt(password.encrypted, key = PKI.load.key(file = keyFile))
  )
  
  ### NA if no password is stored  
}
