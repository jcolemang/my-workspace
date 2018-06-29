{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import qualified Data.Map                     as Map
import           Graphics.X11.ExtraTypes.XF86
import           XMonad
import qualified XMonad.StackSet              as SS
import XMonad.Actions.Submap
import XMonad.Actions.WindowMenu
import XMonad.Hooks.DynamicLog


-- ~~~~~ My Configuration ~~~~~

-- basic configuration variables
myFocusFollowsMouse  = False
myModMask            = mod4Mask  -- rebind mod to windows key
myFocusedBorderColor = "#641588" -- purple
myNormalBorderColor  = "#000000" -- black
myTerminal           = "termite"
startEmacs           = "emacs --daemon"
browser              = "firefox"
appSearch            = "dmenu_run" -- rebind mod to windows key
music                = "spotify"   -- code to skip songs relies on this
editor               = "emacsclient -nc"
files                = "dolphin"
screenshot           = "bash -c 'sleep 0.1; gnome-screenshot -a'"

-- used command from https://ubuntuforums.org/showthread.php?t=1797848
nextSong             = "dbus-send --print-reply " ++
                       "--dest=org.mpris.MediaPlayer2.spotify " ++
                       "/org/mpris/MediaPlayer2 " ++
                       "org.mpris.MediaPlayer2.Player.Next"
previousSong         = "dbus-send --print-reply " ++
                       "--dest=org.mpris.MediaPlayer2.spotify " ++
                       "/org/mpris/MediaPlayer2 " ++
                       "org.mpris.MediaPlayer2.Player.Previous"
pauseSong            = "dbus-send --print-reply " ++
                       "--dest=org.mpris.MediaPlayer2.spotify " ++
                       "/org/mpris/MediaPlayer2 " ++
                       "org.mpris.MediaPlayer2.Player.Pause"
playSong             = "dbus-send --print-reply " ++
                       "--dest=org.mpris.MediaPlayer2.spotify " ++
                       "/org/mpris/MediaPlayer2 " ++
                       "org.mpris.MediaPlayer2.Player.Play"
resetTime            = "sudo ntpd -gq"

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


myManageHook = composeAll
  [ title =? browser --> doShift webWorkspace ]


myKeys conf = Map.fromList $

  let lowerVolume
        = spawn "pactl set-sink-volume alsa_output.pci-0000_00_1b.0.analog-stereo -5%"
      raiseVolume
        = spawn "pactl set-sink-volume alsa_output.pci-0000_00_1b.0.analog-stereo +5%"
      muteVolume
        = "pactl set-sink-mute alsa_output.pci-0000_00_1b.0.analog-stereo toggle"
      dimScreen      = spawn "light -U 5"
      brightenScreen = spawn "light -A 5"
  in

    [ ( (myModMask, xK_c)
      , submap . Map.fromList $
        [ ( (0, xK_s)
          , submap . Map.fromList $
            [ ( (0, xK_e)
              , spawn "echo \"¯\\\\_(ツ)_/¯\" | xclip" )
            ] )
        ] )
    ]
    
    ++ 

  -- opening applications
    [ ( (myModMask, xK_o)
      , submap . Map.fromList $
        [ ( (0, xK_e)
          , spawn editor
          )
        , ( (0, xK_m)
          , spawn music
          )
        , ( (0, xK_w)
          , spawn browser
          )
        , ( (0, xK_f)
          , spawn files
          )
        , ( (0, xK_t)
          , spawn $ XMonad.terminal conf
          )
        ]
      )

    , ( (myModMask, xK_t)
      , submap . Map.fromList $
      [ ( (0, xK_t)
        , submap . Map.fromList $
        [ ( (0, xK_p)
          , submap . Map.fromList $
          [ ( (0, xK_d)
            , spawn "xinput --disable 12"
            )
          , ( (0, xK_e)
            , spawn "xinput --enable 12"
            )
          ] )
        ] )
      ] )

    , ( (myModMask, xK_m)
      , submap . Map.fromList $
        [ ( (0, xK_n)
          , spawn nextSong
          )
        , ( (0, xK_b)
          , spawn previousSong
          )
        , ( (0, xK_p)
          , spawn pauseSong
          )
        , ( (0, xK_s)
          , spawn playSong
          )
        ]
      )

    , ( (myModMask, xK_Return)
      , spawn $ XMonad.terminal conf )

    , ( (myModMask .|. shiftMask, xK_e)
      , spawn startEmacs )

    , ( (myModMask, xK_s)
      , spawn appSearch )
    
    , ( (0, xK_Print)
      , spawn screenshot )
    , ( (myModMask, xK_backslash)
      , spawn screenshot )

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
      , windows SS.focusDown )

    , ( (myModMask, xK_b)
      , windows SS.focusUp )

    , ( (myModMask .|. shiftMask, xK_t)
      , windows SS.swapUp )

    , ( (myModMask .|. shiftMask, xK_h)
      , windows SS.swapDown )

    ]

    ++

    -- XMonad control
    [ ( (myModMask .|. shiftMask, xK_c)
      , kill )

    , ( (myModMask, xK_q)
      , do broadcastMessage ReleaseResources
           restart "myxmonad" True )

    , ( (myModMask .|. shiftMask, xK_q)
      , spawn "pkill myxmonad" )

    ]

    ++

    -- OS control
    [ ( (myModMask .|. shiftMask, xK_slash)
      , dimScreen )
    , ( (0, xF86XK_MonBrightnessDown)
      , dimScreen )

    , ( (myModMask .|. shiftMask, xK_backslash)
      , brightenScreen )
    , ( (0, xF86XK_MonBrightnessUp)
      , brightenScreen )

    , ( (0, xF86XK_AudioLowerVolume)
      , lowerVolume )
    , ( (0, xF86XK_AudioRaiseVolume)
      , raiseVolume )

    , ( (0, xF86XK_AudioNext)
      , spawn nextSong )

    , ( (0, xF86XK_AudioPrev)
      , spawn previousSong )

    , ( (0, xF86XK_AudioPlay)
      , spawn playSong
      )

    , ( (myModMask, xF86XK_AudioPlay)
      , spawn pauseSong
      )

    , ( (0, xF86XK_AudioMute)
      , spawn muteVolume )

    , ( (myModMask, xK_k)
      , spawn "~/.xmonad/scripts/layout.sh" )

    , ( (myModMask, xK_g)
      , submap . Map.fromList $
        [ ( (0, xK_t)
          , submap . Map.fromList $
            [ ( (0, xK_s)
              , spawn "systemctl suspend" )
            ] )
        ] )
           
    , ( (myModMask, xK_d)
      , submap . Map.fromList $
      [ ( (0, xK_v)
        , submap . Map.fromList $
          [ ( (0, xK_g)
            , submap . Map.fromList $
              [ ( (0, xK_a)
                , submap . Map.fromList $
                  [ ( (0, xK_n)
                    , spawn "xrandr --output VGA-1 --auto"
                    )
                  , ( (0, xK_f)
                    , spawn "xrandr --output VGA-1 --off"
                    )
                  ] )
              ] )
          ] )
      , ( (0, xK_e)
        , submap . Map.fromList $
          [ ( (0, xK_d)
            , submap . Map.fromList $
              [ ( (0, xK_p)
                , submap . Map.fromList $
                  [ ( (0, xK_n)
                    , spawn "xrandr --output eDP-1 --auto"
                    )
                  , ( (0, xK_f)
                    , spawn "xrandr --output eDP-1 --off"
                    )
                  ] )
              ] )
          ] )
      ] )

    , ( (myModMask, xK_i)
      , submap . Map.fromList $
      [ ( (0, xK_s)
        , submap . Map.fromList $
          [ ( (0, xK_h)
            , spawn "sudo netctl stop-all; sudo netctl start home"
            )
          , ( (0, xK_s)
            , spawn "sudo netctl stop-all; sudo netctl start school"
            )
          ] )
      ] )

    , ( (myModMask, xK_r)
      , submap . Map.fromList $
      [ ( (0, xK_s)
        , submap . Map.fromList $
        [ ( (0, xK_t)
          , spawn "sudo ntpd -gq"
          )
        ] )
      ] )
    ]

  ++

  -- keybindings for workspaces
    [ ( (mask .|. myModMask, key)
      , windows $ func workspace)
    | (workspace, key) <- zip myWorkspaces numPadKeys
    , (func, mask)     <- [ (SS.view, 0)
                          , (SS.shift, shiftMask)
                          ]
    ]

numPadKeys = [ xK_KP_End    ,xK_KP_Down , xK_KP_Page_Down -- 1, 2, 3
             , xK_KP_Left   ,xK_KP_Begin, xK_KP_Right     -- 4, 5, 6
             , xK_KP_Home   ,xK_KP_Up   , xK_KP_Page_Up   -- 7, 8, 9
             , xK_KP_Insert                             --    0
             ]

myStartupHook = do
  spawn "autocutsel -fork &"
  spawn "autocutsel -selection PRIMARY -fork &"

main :: IO ()
main =
  launch def
  { terminal           = myTerminal
  , focusFollowsMouse  = myFocusFollowsMouse
  , modMask            = myModMask
  , workspaces         = myWorkspaces
  , manageHook         = myManageHook
  , logHook            = dynamicLog
  , keys               = myKeys
  , normalBorderColor  = myNormalBorderColor
  , focusedBorderColor = myFocusedBorderColor
  , startupHook        = myStartupHook
  }
