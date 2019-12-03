module Helpers where

chunks:: [Int] -> [a] -> [[a]]
chunks [] _ = []
chunks _ [] = []
chunks (n:nums) list = front : (chunks nums back)
    where (front, back) = splitAt n list
	