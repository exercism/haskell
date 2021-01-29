module Deque (Deque, mkDeque, pop, push, shift, unshift) where

data Deque a = Dummy

mkDeque :: IO (Deque a)
mkDeque = error "You need to implement this function."

pop :: Deque a -> IO (Maybe a)
pop deque = error "You need to implement this function."

push :: Deque a -> a -> IO ()
push deque x = error "You need to implement this function."

unshift :: Deque a -> a -> IO ()
unshift deque x = error "You need to implement this function."

shift :: Deque a -> IO (Maybe a)
shift deque = error "You need to implement this function."
