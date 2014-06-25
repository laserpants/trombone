{-# LANGUAGE OverloadedStrings #-}
module Trombone.Tests.Bootstrap 
    ( runTests
    ) where

import Trombone.Db.Template
import Trombone.RoutePattern

runTests :: IO ()
runTests = print [ routePatternTest1
                 , routePatternTest2
                 , routePatternTest3
                 , routePatternTest4
                 , dbTemplateTest1
                 ]

-------------------------------------------------------------------------------
-- Db.Template
-------------------------------------------------------------------------------

dbTemplateTest1 = Right "select id from dispatch where id = \"5\""
                == instantiate template [(":id", EscapedText "\"5\"")] 
  where template = DbTemplate [ DbSqlStatic "select id from dispatch where id = "
                              , DbSqlUriParam "id" ]

-------------------------------------------------------------------------------
-- RoutePattern
-------------------------------------------------------------------------------

routePatternTest1 = Params [("id", "5")] 
                  == match (decompose "/product/order/:id/") 
                        ["product", "order", "5"]

routePatternTest2 = RoutePattern [Atom "a", Variable "b", Atom "c"] 
                  == decompose "/a/:b/c/"

routePatternTest3 = Params [("name", "bob"), ("order", "11")] 
                  == match (decompose "/customer/:name/order/:order/show") 
                        ["customer", "bob", "order", "11", "show"]

routePatternTest4 = NoMatch
                  == match (decompose "/customer/:name/order/:order/show") 
                        ["customer", "bob", "order", "11"]

