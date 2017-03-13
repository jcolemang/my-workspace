{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import qualified Data.Map                     as Map
import           Graphics.X11.ExtraTypes.XF86
import           XMonad
import           XMonad.Hooks.SetWMName
import qualified XMonad.StackSet              as SS


-- ~~~~~ Helper Functions ~~~~~

-- getNextWrap :: [a] -> Maybe a
-- getNextWrap [] = Nothing
-- getNextWrap [x] = Just x
-- getNextWrap (_:x:_) = Just x


-- otherScreen = SS.screens

-- ~~~~~ My Configuration ~~~~~

-- basic configuration variables
myFocusFollowsMouse  = False
myModMask            = mod4Mask -- rebind mod to windows key
myFocusedBorderColor = "#641588"
myNormalBorderColor  = "#FF0000"
myTerminal           = "terminator"
startEmacs           = "emacs --daemon"
browser              = "google-chrome-stable"
appSearch            = "dmenu_run" -- rebind mod to windows key
music                = "spotify" -- code to skip songs relies on this
editor               = "emacsclient -nc"
files                = "nautilus"


-- script to set things like keyboard config
myStartupHook = do
  setWMName "LG3D"
  spawn "~/.xmonad/scripts/startup.sh"



-- workspace names
consoleWorkspace = "1:console"
emacsWorkspace   = "2:emacs"
codeWorkspace    = "3:code"
webWorkspace     = "4:web"
vmWorkspace      = "5:vm"
mediaWorkspace   = "6:media"


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
  [ className =? browser --> doShift webWorkspace ]


myKeys conf = Map.fromList $

  -- opening applications
  [ ( (myModMask, xK_Return)
    , spawn $ XMonad.terminal conf )

  , ( (myModMask, xK_e)
    , spawn editor )

  , ( (myModMask .|. shiftMask, xK_e)
    , spawn startEmacs )

  , ( (myModMask, xK_w)
    , spawn browser )

  , ( (myModMask, xK_s)
    , spawn appSearch )

  , ( (myModMask, xK_m)
    , spawn music )

  , ( (myModMask, xK_f)
    , spawn files )

  ]

  ++

  -- window control
  [ ( (myModMask, xK_period)
    , sendMessage (IncMasterN 1) )

  , ( (myModMask, xK_comma)
    , sendMessage (IncMasterN (-1)) )

  , ( (myModMask, xK_h)
    , sendMessage Shrink )

  , ( (myModMask, xK_l)
    , sendMessage Expand )

  , ( (myModMask, xK_space)
    , sendMessage NextLayout )

  , ( (myModMask, xK_n)
    , windows SS.focusUp )

  , ( (myModMask, xK_b)
    , windows SS.focusUp )

  -- , ( (myModMask, xK_o)
  --   , otherScreen )

  ]

  ++

  -- XMonad control
  [ ( (myModMask .|. shiftMask, xK_c)
    , kill )

  , ( (myModMask, xK_q)
    , broadcastMessage ReleaseResources >> restart "xmonad" True )

  , ( (myModMask .|. shiftMask, xK_q)
    , spawn "pkill xmonad-x86_64-l" )

  ]

  ++

  -- OS control
  [ ( (myModMask .|. shiftMask, xK_slash)
    , dimScreen )
  , ( (myModMask .|. shiftMask, xK_backslash)
    , brightenScreen )

  , ( (myModMask, xK_slash)
    , lowerVolume )
  , ( (myModMask, xK_backslash)
    , raiseVolume )
  , ( (0, xF86XK_AudioLowerVolume)
    , lowerVolume )
  , ( (0, xF86XK_AudioRaiseVolume)
    , raiseVolume )

  -- used command from https://ubuntuforums.org/showthread.php?t=1797848
  , ( (0, xF86XK_AudioNext)
    , spawn $
      "dbus-send --print-reply " ++
      "--dest=org.mpris.MediaPlayer2.spotify " ++
      "/org/mpris/MediaPlayer2 " ++
      "org.mpris.MediaPlayer2.Player.Next" )

  , ( (0, xF86XK_AudioPrev)
    , spawn $
      "dbus-send --print-reply " ++
      "--dest=org.mpris.MediaPlayer2.spotify " ++
      "/org/mpris/MediaPlayer2 " ++
      "org.mpris.MediaPlayer2.Player.Previous" )

  , ( (0, xF86XK_AudioPlay)
    , spawn $
      "dbus-send --print-reply " ++
      "--dest=org.mpris.MediaPlayer2.spotify " ++
      "/org/mpris/MediaPlayer2 " ++
      "org.mpris.MediaPlayer2.Player.Play" )

  , ( (myModMask, xF86XK_AudioPlay)
    , spawn $
      "dbus-send --print-reply " ++
      "--dest=org.mpris.MediaPlayer2.spotify " ++
      "/org/mpris/MediaPlayer2 " ++
      "org.mpris.MediaPlayer2.Player.Pause" )

  ]

  ++

  -- keybindings for workspaces
  [ ( (mask .|. myModMask, key)
    , windows $ func workspace)
  | (workspace, key) <- zip myWorkspaces numPadKeys
  , (func, mask)     <- [ (SS.greedyView, 0)
                        , (SS.shift, shiftMask)
                        ]
  ]

  where lowerVolume    = spawn "amixer -q sset Master 5%-"
        raiseVolume    = spawn "amixer -q sset Master 5%+"
        dimScreen      = spawn "xbacklight -dec 5"
        brightenScreen = spawn "xbacklight -inc 5"


numPadKeys = [ xK_KP_End    ,xK_KP_Down , xK_KP_Page_Down -- 1, 2, 3
             , xK_KP_Left   ,xK_KP_Begin, xK_KP_Right     -- 4, 5, 6
             , xK_KP_Home   ,xK_KP_Up   , xK_KP_Page_Up   -- 7, 8, 9
             , xK_KP_Insert                             --    0
             ]


main :: IO ()
main = launch def
  { terminal           = myTerminal
  , focusFollowsMouse  = myFocusFollowsMouse
  , startupHook        = myStartupHook
  , modMask            = myModMask
  , workspaces         = myWorkspaces
  , manageHook         = myManageHook
  , keys               = myKeys
  , normalBorderColor  = myNormalBorderColor
  , focusedBorderColor = myFocusedBorderColor
  }
