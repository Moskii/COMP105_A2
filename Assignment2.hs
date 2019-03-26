
type Transaction = (Char, Int, Int, String, Int)

test_log :: [Transaction]
test_log = [('B', 100, 1104,  "VTI",  1),
            ('B', 200,   36, "ONEQ",  3),
            ('B',  50, 1223,  "VTI",  5),
            ('S', 150, 1240,  "VTI",  9),
            ('B', 100,  229, "IWRD", 10),
            ('S', 200,   32, "ONEQ", 11),
            ('S', 100,  210, "IWRD", 12)
            ]

transaction_to_string :: Transaction -> String
transaction_to_string () = ""
transaction_to_string (action,unit,price,stock,day) =
    let
      finalAction = if action == 'B' then "Bought" else "Sold"
    in
      --The show function converts the integers into a string
      finalAction ++ " " ++ (show unit) ++ " " ++ "units of " ++ stock ++ " for " ++ (show price) ++ " pounds each on day " ++ (show day)
      --Concatanates everything into one big string.

trade_report_list :: [Transaction] -> [String]
--Converts the user list into a string, then forming it into a list, using the function from before
trade_report_list list = map transaction_to_string list

stock_test :: String -> Transaction -> Bool
-- the string is compared to the stock to determine whether its equal
stock_test str (action,unit,price,stock,day) = if str == stock then True else False


get_trades :: String -> [Transaction] -> [Transaction]
-- Implemented an anomynous function which focuses on the stock 'attribute' and compares it with str. Then filters it with this.
get_trades str list = filter (\(action,unit,price,stock,day) -> stock == str) list


trade_report :: String -> [Transaction] -> String
trade_report str list =
    let
      -- Recieve all trades with that specific string.
      -- Then convert 'all_trades' into a string AND into a list
      all_trades = get_trades str list
      report = trade_report_list all_trades
    in
      -- Shows the line breaks.
      unlines report

update_money :: Transaction -> Int -> Int
update_money (action,unit,price,stock,day) money =
    if
      -- if action is equal to B, money would be subtracted otherwise added
      action == 'B'
    then
      money - (unit * price)
    else
      money + (unit * price)


profit :: [Transaction] -> String -> Int
profit list str = 
    let
      -- Using the function from Part A to gather relevant transactions
      relevant_trades = get_trades str list
      finalProfit = foldr (\(action,unit,price,stock,day) acc -> if action == 'B' then acc - (unit * price) else acc + (unit * price)) 0 relevant_trades
    in
      finalProfit


profit_report :: [String] -> [Transaction] -> String
-- Base cases
profit_report [] _ = []
profit_report _ [] = []
profit_report (x:xs) list =
    let
      final_profit = profit list x
      conv = show final_profit
    in
      x ++ ": " ++ conv ++ "\n" ++ profit_report xs list

test_str_log = "BUY 100 VTI 1\nBUY 200 ONEQ 3\nBUY 50 VTI 5\nSELL 150 VTI 9\nBUY 100 IWRD 10\nSELL 200 ONEQ 11\nSELL 100 IWRD 12\n"

type Prices = [(String, [Int])]

test_prices :: Prices
test_prices = [
                ("VTI", [1689, 1785, 177, 1765, 1739, 1725, 1615, 1683, 1655, 1725, 1703, 1726, 1725, 1742, 1707, 1688, 1697, 1688, 1675]),
                ("ONEQ", [201, 203, 199, 199, 193, 189, 189, 183, 185, 190, 186, 182, 186, 182, 182, 186, 183, 179, 178]),
                ("IWRD", [207, 211, 213, 221, 221, 222, 221, 218, 226, 234, 229, 229, 228, 222, 218, 223, 222, 218, 214])
              ]
lines_function :: String -> [String]
-- Splitting the string into a suitable format.
lines_function str = lines str

split_prices :: (String, [Int]) -> [Int]
-- Takes an example of a test price
-- Using the snd function, it returns the integers as I wil use !! function to determine the exact price on a specific day
split_prices tuple = 
    let
      ints = snd tuple
    in
      ints

specific_price :: (String, [Int]) -> String -> Int
-- Takes ONE tuple containing the stock and a list of prices. Then ONE string.
specific_price tuple str =
    let
      -- converts the string format
      -- The 4th element (3rd to haskell) will represent the day
      -- converts the string into an integer
      -- displays all the prices from the tuple
      -- Using !! function, displays the price for that day.
      worded = words str
      day = worded !! 3
      conv = read day
      list_price = split_prices tuple
      final_price = list_price !!(conv-1)
    in
      final_price 

finding_Profit :: (String, [Int]) -> String -> Int
-- ONE tuple and ONE string
finding_Profit tuple str =
    let
      worded = words str
      -- Calls the function created before to find the exact price for that day
      specPrice = specific_price tuple str
      unit = worded !! 1
      conversion = read unit
      -- The Unit multiplied by the price on that day.
      profit = conversion * specPrice
    in
      profit

split_stock :: (String, [Int]) -> String
-- Takes an example of a test price
-- Using the fst function, returns the stock. 
split_stock tuple = 
    let
      str = fst tuple
    in
      str

complex_profit_report :: String -> Prices -> String
complex_profit_report = error "Not implemented" 
-- TBC
