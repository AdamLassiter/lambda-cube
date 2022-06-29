-- | Safe and unsafe logging functions wrapping Control.Logging
module L3.Log.Colors where

reset :: String
reset = "\x001b[0m"

black :: String
black = "\x001b[30m"

red :: String
red = "\x001b[31m"

green :: String
green = "\x001b[32m"

yellow :: String
yellow = "\x001b[33m"

blue :: String
blue = "\x001b[34m"

magenta :: String
magenta = "\x001b[35m"

cyan :: String
cyan = "\x001b[36m"

white :: String
white = "\x001b[37m"

brightBlack :: String
brightBlack = "\x001b[30;1m"

brightRed :: String
brightRed = "\x001b[31;1m"

brightGreen :: String
brightGreen = "\x001b[32;1m"

brightYellow :: String
brightYellow = "\x001b[33;1m"

brightBlue :: String
brightBlue = "\x001b[34;1m"

brightMagenta :: String
brightMagenta = "\x001b[35;1m"

brightCyan :: String
brightCyan = "\x001b[36;1m"

brightWhite :: String
brightWhite = "\x001b[37;1m"
