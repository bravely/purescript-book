module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head)
import Data.Maybe (Maybe, isJust)

type Address =
  { street :: String
  , city   :: String
  , state  :: String
  }

type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }

type AddressBook = List Entry

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <>
                   addr.city <> ", " <>
                   addr.state

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <>
                  entry.firstName <> ": " <>
                  showAddress entry.address

emptyBook :: AddressBook
emptyBook = empty

-- -- This line should have been automatically deleted by resetSolutions.sh. See Chapter 2 for instructions. NOTE TO MAINTAINER: If editing `insertEntry`, remember to also update the non-anchored (and unsimplified) version of this function that is hardcoded in the book text.
insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry entry = Cons entry

-- -- This line should have been automatically deleted by resetSolutions.sh. See Chapter 2 for instructions. NOTE TO MAINTAINER: If editing `findEntry`, remember to also update the non-anchored (and unsimplified) version of this function that is hardcoded in the book text.
-- findEntry :: String -> String -> AddressBook -> Maybe Entry
-- findEntry firstName lastName = head <<< filter filterEntry
--   where
--   filterEntry :: Entry -> Boolean
--   filterEntry entry = entry.firstName == firstName && entry.lastName == lastName
findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName book =
  book
  # filter filterEntry
  # head
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street book =
  book
  # filter (_.address.street >>> eq street)
  # head

isInBook :: String -> String -> AddressBook -> Boolean
isInBook firstName lastName book =
  isJust (findEntry firstName lastName book)

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates book = book # filter (not <<< isDuplicate)
  where
  isDuplicate :: Entry -> Boolean
  isDuplicate entry =
    isJust (findEntry (entry.firstName) (entry.lastName) book)
