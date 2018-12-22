splitMessage =
function(x, attachments = TRUE, filename = "")
{
 filename = ""

   # If the given string is actually a file name,
   # then read its contents. This is just
   # a convenience to allow us to test this
   # code interactivelyand directly without
   # calling it indirectly via other functions.
 if(length(x) == 1 && file.exists(x)) {
   filename = x
   x = readLines(x, encoding = "latin1")
 }

  # Have to handle any files that aren't actually mesages.
  # Occurs in the actual Spam Assassin corpus.
 if(length(grep("^mv ", x[1]))) {
   warning(paste(filename, "is not a mail message"))
   return(NULL)
 }

  # Get the first empty line.
  el = match("", x)
  # Get the header as being the first through just before
  # this blank line and pass it to the function that converts
  # it into a named vector.
  header = processHeader(x[1:(el-1)]) 

   # The body is everything past the first blank line.
   # So its also x[- c(1:(el-1)) ]
  message = x[(el+1):length(x)]

   # If we are doing the full attachment thing, process the body.
  if(attachments) 
    message = splitBody(message, header = header, filename = filename)


   # Return an object with two fields, the header and the message.
   # This preserves each as a separate object.
  return(list(header=header, body=message))
}


processHeader =
  #
  # Takes the lines in the header of a message or attachment and
  # brings the continuation lines to their "parent" lines
  # and then breaks these
  #     name: value
  # lines into a named vector of values with names given by the
  # {name} vector.
  #
  # This works for both the message header and the
  # header for each attachment.
  #
function(h)
{
    # Handle the special case where the header is empty.
    # This occurs in some of the attachments.
#   if(length(h) == 0 || all(h==""))
#     return(character(0))

    # Handle any peculiar first lines of the form
    #    From bob@bob.org
    # by replacing a
    # From at the beginning of the line with
    # X-From: to make it look like the other name: value
    # elements.
   h[1] = gsub("^From", "X-From:", h[1])

    # Now find the continuation lines.
    # We are looking for space or TAB characters in the first
    # character, i.e.  a beginning line followed by a
    # space or a TAB.  TAB is identified by \t.
    # We can also find these lines without regular expressions using
    #  (1:length(h))[!is.na(match(substring(h, 1, 1),  c(" ", "\t")))]
    # grep is easier when we know about regular expressions.
   continuations = grep("^[ \t]", h, perl = TRUE)
   if(length(continuations) > 0) {
      # So we have continuation lines.
      # Loop over them - backwards - and paste
      # the line to the one before it.
      # When we see two consecutive continuation lines
      # e.g.  To: duncan
      #         bob
      #         jane
      # we will paste "jane" to "bob", and then
      # that new line ("bob jane") to "duncan"
      # and get what we want.
     for(i in rev(continuations)) {
       h[i-1] = paste(h[i-1], h[i], sep="\n")
     }

     # And now that we have combined the continuation lines
     # to their parent lines, we need to drop them. So we
     # exclude them from our vector.
     h = h[-1*continuations]
   } 
   

   # Now that we have fixed up the continuation lines, we have
   # simple lines of the form
   #      name: value
   # So we turn these into a named vector
   # To do this, we can split each string at the ':'
   # to get the name and the value.
   # Any value that has a ':' in it will be broken
   # into two or more parts (as many ':' as there are) in the value
   # and so we need to put these back together when storing
   # the value.
   # Note that the pattern for strsplit is a regular expression.
   x = strsplit(h, ":")

    # Put any values that contained : back together again.
    # by taking everything but the name and pasting the elements
    # together.
   header = sapply(x, function(x) {
                         paste(x[-1], collapse=":")
                      })
    # Names are just the first elements of the split.
   names(header) = sapply(x, function(x) x[1])


    # Degenerate cases may leave us with an empty list.
    # In this case, just return an empty vector. This is
    # more for attachment headers.
   if(is.list(header) && length(header) == 0)
     header = character()
   
   header
}



#
# This function iterates over the names of the message files
# within the specified directory and prcess them into
# S objects that represent the different parts of the message
# and whether it is spam or not.
#

readMessages =
function(files = list.files(pattern = "^[0-9]*.", path = dir, full.names = TRUE),
          dir = ".", isSpam = FALSE, verbose = TRUE, ...)
{
  if(any(files == ""))
    stop("No file name specified for messages")

   # Note that we pass the ... through the lapply and on to splitMessage.
   # We don't interpret them here. 
  messages = lapply(files, function(i, ...) {
                             if(verbose)
                               cat(i, "\n")
                             el = splitMessage(i, ..., filename = i)
                             el$spam = isSpam
                             el
                           }, ...)

    # We make the return value "invisible' so that it won't print
    # on the console if we call this function directly from the prompt
    # and don't assign the value. If we don't do this, we will get
    # a lengthy display.
   invisible(messages)
}

# These are the names of the directories.
Directories = c("easy_ham", "easy_ham_2", "hard_ham", "spam", "spam_2")

readAllMessages =
function(dirs = system.file("messages", Directories, package = package),
          package = "RSpamData", verbose = 0, ...)
{
   # The package argument allows us to specify whether we want the
   # RSpamData or RSpamDataMini package.

  
   # We loop over the directories and apply a simply function that we define here
   # for simplicity. The function announces its action if verbose is TRUE
   # and then determines from the name of the directory whether the messages in
   # that directory are HAM or SPAM.
   # The return value from readMessage is a list with an element for each message
   # in the directory.

 ans = lapply(dirs, function(i, ...) {
                     if(as.logical(verbose))
                       cat("Directory:" , i, "\n")
                     isSpam = (length(grep("spam", i)) > 0)
                     readMessages(dir = i, verbose = verbose > 0, 
                                  isSpam = isSpam, ...)
                    })
  
  # Now we have a list with as many elements as there are Directories.
  # Since we just want all the message objects, we want to get rid of
  # this extra layer of hierarchical structure. So we "unravel"
  # this list and put all the > 9,000 messages into a single
  # list using  unlist().  We have to ensure that we don't unravel
  # all the objects by first unravelling this list and then the next
  # layer and so on. So we use recursive = FALSE. Without this, we would
  # end up simply with a very large character vector containing all the
  # text from the mail messages.
 invisible(unlist(ans, recursive = FALSE))
}  



#########################################################################
# These additional functions are responsible for breaking the 
# body of the message into is different elements. Specifically,
# it separates the body into the different attachments.
# What we get back for each body is an S object containing
#  the plain text of the body  (named text)
#  and a list of each of the attachments.
# Each attachment is a pair of values/objects, namely
# the header for the attachment describing its type, etc.
# and the content of the attachment. These are combined in
# a list() with names header and text respectively.


splitBody = 
 #
 # Takes a message and its (processed) header
 # and returns the message broken into 
 # its different attachment elements.
 # The input should be in lines, i.e. a character vector
 # of the lines in the body
 # and the header shoyuld be pre-processed to be a 
 # named character vector of entries.
 #
function(message,  boundary = NULL, header, filename="") 
{
   # If the boundary string is not explicitly specified,
   # compute it from the header information.
 if(is.null(boundary)) {
    if("Content-Type" %in% names(header))
      boundary = getBoundary(header["Content-Type"])
 }

   # if there is no boundary at this point, then
   # the body is just a simple message and there
   # are no attachments. So just return the body.
 if(is.null(boundary) || is.na(boundary))
   return(list(text = message))


   #  So we are dealing with a message with one or more attachments.
   # We need to find the start and end points that surround or
   # bracket the attachment components in the message.
   # An attachment starts with
   # --<boundary string>
   # and ends with either
   #   the start of another attachment
   #   the string --<boundary string>--
   #
   # The message will be made up of  the regular text which is everything
   # not in an attachment and then the individual attachment elements.
   # 
 prefix = paste("--", boundary, sep = "")

 starts = (1:length(message))[!is.na(match(message, prefix))]
 ends  = (1:length(message))[!is.na(match(message, paste(prefix, "--", sep = "")))]


  # Handle degenerate cases.
 if(length(starts) == 1 && length(ends) == 0)
     ends = length(message)
 
 pos = sort(c(starts, ends))

 if(length(pos) == 0) {
   warning("Message (", filename, ") with boundary for multipart attachments, but can't find match: ", boundary)
   return(list(text = message))
 }

  # The regular text is everything that is not in an attachment.
 text = message[ - seq(min(pos), max(pos))]
 
 attachments = list()
 for(i in 1:(length(pos)-1)) {
   which = (pos[i] + 1):(pos[i+1]-1)
   attachments[[i]] = processAttachment(message[ which])
 }

 list(text = text, attachments = attachments)
}

processAttachment =
function(lines)
{
   breakPoint = match("", lines)
   header = lines[1:(breakPoint-1)]

   x = processHeader(header)
   header = sapply(x, function(x) {
                        x = gsub("^[ \t]*(.*)[ \t;]*$", "\\1", x)
#XXX
                        x = gsub(";$", "", x)
                        x
                      }
                  )
#   names(header) = sapply(x, function(x) x[1])

   if("Content-Type" %in% names(header)) {
     tmp = strsplit(header["Content-Type"], "/")[[1]][2]
     header["Type"] = gsub(";.*", "", tmp)
   }
  list(header = header, text = lines[-c(1:breakPoint)])
}

getBoundary =
#
# This extracts the boundary identifier string
# given in the type variable
# that should be obtained from
# the Content-Type entry in the header.
function(type)
{
  if(length(names(type)) > 0 && "Content-Type" %in% names(type) )
     type = type["Content-Type"]
  
  type = gsub('"', "", type)

  boundary = rep(NA, length(type))
  which = grep(" multipart", type)
  if(length(which)) {
     # allow for space after the = sign
    boundary[which] = gsub(".*boundary= *([^;]*);?.*", "\\1", type[which])
  }

  boundary
}

