{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import XMonad
import qualified XMonad.StackSet as SS
import qualified Data.Map as Map
-- import XMonad.Util.EZConfig

-- ~~~~~ Helper Functions ~~~~~

brightnessDown = 232
brightnessUp = 233

-- ~~~~~ My Configuration ~~~~~

-- basic configuration variables
myTerminal = "terminator"
myFocusFollowsMouse = False
myBrowser = "google-chrome-stable"
myModMask = mod4Mask -- rebind mod to windows key
myAppSearch = "dmenu_run" -- rebind mod to windows key
myFocusedBorderColor = "0x641588"
myNormalBorderColor = "0xFF0000"

-- script to set things like keyboard config
myStartupHook = spawn "~/.xmonad/scripts/startup.sh"

-- workspace names
consoleWorkspace = "1:console"
emacsWorkspace =  "2:emacs"
codeWorkspace =  "3:code"
webWorkspace = "4:web"
vmWorkspace = "5:vm"
mediaWorkspace = "6:media"


myWorkspaces :: [String]
myWorkspaces =
  let named = [ consoleWorkspace
              , emacsWorkspace
              , codeWorkspace
              , webWorkspace
              , vmWorkspace
              , mediaWorkspace
              ]
  in named ++ map show [(length named)..10]


myManageHook = foldl mappend mempty
  [ className =? myBrowser --> doShift webWorkspace ]


myKeys conf = Map.fromList $

  -- opening applications
  [ ((myModMask, xK_Return), spawn $ XMonad.terminal conf)
  , ((myModMask, xK_e), spawn "emacsclient -nc")
  , ((myModMask .|. shiftMask, xK_e), spawn "emacs --daemon")
  , ((myModMask, xK_w), spawn myBrowser)
  , ((myModMask, xK_s), spawn myAppSearch)
  ]

  ++

  -- window control
  [ ((myModMask, xK_period), sendMessage (IncMasterN 1))
  , ((myModMask, xK_comma), sendMessage (IncMasterN (-1)))
  , ((myModMask, xK_h), sendMessage Shrink)
  , ((myModMask, xK_l), sendMessage Expand)
  , ((myModMask, xK_space), sendMessage NextLayout)
  , ((myModMask, xK_Tab), setTopFocus)
  ]

  ++

  -- XMonad control
  [ ((myModMask .|. shiftMask, xK_c), kill)
  , ((myModMask, xK_q), broadcastMessage ReleaseResources >> restart "xmonad" True)
  , ((myModMask .|. shiftMask, xK_q), spawn "pkill xmonad-x86_64-l")
  ]

  ++

  -- OS control
  [ ((myModMask .|. shiftMask, xK_slash), spawn "xbacklight -dec 5")
  , ((myModMask .|. shiftMask, xK_backslash), spawn "xbacklight -inc 5")
  , ((myModMask, xK_slash), spawn "amixer -q sset Master 5%+")
  , ((myModMask, xK_backslash), spawn "amixer -q sset Master 5%-")
  ]

  ++

  -- keybindings for workspaces
  [ ((mask .|. myModMask, key)
    , windows $ func workspace) | (workspace, key) <- zip myWorkspaces numPadKeys
                                , (func, mask) <- [ (SS.greedyView, 0)
                                                  , (SS.shift, shiftMask)
                                                  ]
                                ]


numPadKeys = [ xK_KP_End,  xK_KP_Down,  xK_KP_Page_Down -- 1, 2, 3
             , xK_KP_Left, xK_KP_Begin, xK_KP_Right     -- 4, 5, 6
             , xK_KP_Home, xK_KP_Up,    xK_KP_Page_Up   -- 7, 8, 9
             , xK_KP_Insert
             ]


main :: IO ()
main = xmonad def
  { terminal = myTerminal
  , focusFollowsMouse = myFocusFollowsMouse
  , startupHook =  myStartupHook
  , modMask = myModMask
  , workspaces = myWorkspaces
  , manageHook = myManageHook
  , keys = myKeys
  , normalBorderColor = myNormalBorderColor
  , focusedBorderColor = myFocusedBorderColor
  }
