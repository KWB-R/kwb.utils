# The functions defined in this file need package "PKI"

# generateKeyFile --------------------------------------------------------------

#' Generate a Decryption Key File
#' 
#' @param target full path to the file to which the key shall be written
#' @export
#' 
generateKeyFile <- function(target)
{
  .checkNamespace("PKI", "generateKeyFile")
  
  PKI::PKI.save.key(key = PKI::PKI.genRSAkey(), target = target) 
}

# .checkNamespace --------------------------------------------------------------

.checkNamespace <- function(packageName, functionName)
{
  if (! requireNamespace(packageName, quietly = TRUE)) {
    
    stop(
      "Please install the package ", hsQuoteChr(packageName), " to use ", 
      "this function: ", hsQuoteChr(functionName), call. = FALSE
    )
  }
}

# createPasswordFile -----------------------------------------------------------

#' Create Encrypted Password File for Account
#' 
#' @param account name of account the user is asked to enter the password for
#' @param keyFile path to the file containing the encryption/decryption key
#' @param passwordFile path to the password file
#' @param password password for account. If \code{NULL} (default) the user will
#'   be asked to enter the password on the console
#' @export
#' 
createPasswordFile <- function(account, keyFile, passwordFile, password = NULL)
{
  .checkNamespace("PKI", "createPasswordFile")
  
  password <- defaultIfNULL(password, .askForPassword(account))
  
  password.encrypted <- PKI::PKI.encrypt(
    charToRaw(password), key = PKI::PKI.load.key(file = keyFile)
  )
  
  writeBin(password.encrypted, passwordFile)
}

# .askForPassword --------------------------------------------------------------

.askForPassword <- function(account = NULL)
{
  clearConsole()

  prompt <- sprintf(
    "Enter password%s: ", 
    if (is.null(account)) "" else paste0("for account '", account, "'")
  )
    
  userInput <- readline(prompt)

  clearConsole()
  
  invisible(userInput)
}

# getPassword ------------------------------------------------------------------

#' Get Encrypted Password from File Using Key
#' 
#' @param passwordFile path to the password file
#' @param keyFile path to the key file
#' @return NA if no password is stored  
#' @export
#' 
getPassword <- function(passwordFile, keyFile) 
{
  .checkNamespace("PKI", "getPassword")
  
  if (! file.exists(passwordFile)) {
    
    stop("Password file '", passwordFile, "' does not exist.")  
  }
  
  password.encrypted <- readBin(passwordFile, what = "raw", n = 256)
  
  invisible(rawToChar(PKI::PKI.decrypt(
    what = password.encrypted, 
    key = PKI::PKI.load.key(file = keyFile)
  )))
}
