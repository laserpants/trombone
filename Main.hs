module Main where

import Trombone.Server

main :: IO ()
main = runWithArgs

-- GET  /user      --  insert into ...
-- GET  /user      >>  select * from user
-- GET  /user/:id  ~>  select * from user where id = {{:id}}
-- GET  /user/:id  ->  select * from user where id = {{:id}}
-- POST /user/     <>  (user, id) insert into user ...
-- GET  /user/:id  ><  update ...
--
-- POST /order     ||  order-create
-- POST /x        <js> abc

-- testParseMethod = do
--     let (Right a) = parse method "" "GET"
--     let (Right b) = parse method "" "POST"
--     let (Right c) = parse method "" "PUT"
--     let (Right d) = parse method "" "PATCH"
--     let (Right e) = parse method "" "DELETE"
--     let (Left  f) = parse method "" "PASTA"
--     print (a, b, c, d, e, f)
-- 
-- testParseUri = do
--     let (Right a) = parse uri "" "/a/:bcd/efg/"
--     let (Right b) = parse uri "" "hello"
--     let (Right c) = parse uri "" "/:one/:two"
--     let (Right d) = parse uri "" "abcd/x"
--     let (Right e) = parse uri "" "/x/"
--     let (Right f) = parse uri "" ":what"
-- --    let (Left  g) = parse uri "" "ab//def"
-- --    let (Left  h) = parse uri "" "@@hello"
-- --    let (Left  i) = parse uri "" "/what?"
--     print (a, b, c, d, e, f)
--  

