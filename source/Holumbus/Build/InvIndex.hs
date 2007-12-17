-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Query.Parser
  Copyright  : Copyright (C) 2007 Sebastian M. Schlatt
  License    : MIT

  Maintainer : Sebastian M. Schlatt (spoogle@schlatt.com)
  Stability  : experimental
  Portability: untested
  Version    : $Id$

  The Holumbus query parser.

-}

-- ----------------------------------------------------------------------------

module Holumbus.Build.InvIndex where

import Text.XML.HXT.Arrow			-- import all stuff for parsing, validating, and transforming XML

import System.IO			   		-- import the IO and commandline option stuff
import System.Environment
import System.Console.GetOpt
import System.Exit
import System

import Data.Maybe
import Data.List

import Holumbus.Index.Inverted
	( empty
	, InvIndex
	)



main	:: IO()
main
    = do
      [rc] <- runX (
      	 	readDocument   [ (a_parse_html ,"1")
      	 				   ] 
					"/home/sms/ruby/ruby0.htm"
		    >>>
		    LA parseDoc
		    >>>
		     writeDocument 	[ (a_output_encoding, isoLatin1)
				   			] 
				   "-"
	 	    >>>
		    getErrStatus
		   )
      exitWith ( if rc >= c_err
		 then ExitFailure 1
		 else ExitSuccess
	       )
	

parseDoc :: XmlTree -> [InvIndex]
parseDoc xt = [empty]