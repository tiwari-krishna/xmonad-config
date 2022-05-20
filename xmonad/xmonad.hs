import XMonad
import Data.Monoid
import System.Exit
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Layout.NoBorders
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import Graphics.X11.ExtraTypes.XF86
--import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Util.NamedScratchpad

import System.IO

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

myTerminal      = "$TERMINAL"
myBrowser       = "$BROWSER"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myBorderWidth   = 2

myModMask       = mod4Mask

myWorkspaces    = ["α","β","γ","δ","ε","ζ","η","θ","ι"]

myNormalBorderColor  = "#141001"
myFocusedBorderColor = "#edeae1"

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm,               xK_Return), spawn $ XMonad.terminal conf)
    , ((modm,               xK_space ), spawn "dmenu_run")
    , ((modm,               xK_q     ), kill)
    , ((modm,               xK_Tab ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_BackSpace   ), windows W.focusDown)

    , ((modm,               xK_j     ), windows W.focusDown)
    , ((modm,               xK_k     ), windows W.focusUp  )
    -- Move focus to the master window
    , ((mod1Mask,           xK_Return), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm .|. controlMask,   xK_Return), windows W.swapMaster)
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    , ((modm,               xK_h     ), sendMessage Shrink)
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
    , ((modm              , xK_p ), sendMessage (IncMasterN 1))
    , ((modm              , xK_o), sendMessage (IncMasterN (-1)))

    , ((modm              , xK_b     ), sendMessage ToggleStruts)

    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modm .|. controlMask, xK_q     ), spawn "xmonad --recompile; xmonad --restart")

    -- Additional Keybindings (Apps specific)
    , ((mod1Mask .|. shiftMask, xK_space  ), spawn "mpc toggle")
    , ((mod1Mask .|. shiftMask, xK_Left  ), spawn "mpc prev")
    , ((mod1Mask .|. shiftMask, xK_Right  ), spawn "mpc next")
    , ((mod1Mask,               xK_Up  ), spawn "pactl set-sink-volume @DEFAULT_SINK@ +3%")
    , ((mod1Mask,               xK_Down  ), spawn "pactl set-sink-volume @DEFAULT_SINK@ -3%")
    , ((mod1Mask,               xK_bracketleft  ), spawn "mpc seek -10")
    , ((mod1Mask .|. shiftMask, xK_bracketleft  ), spawn "mpc seek -60")
    , ((mod1Mask,               xK_bracketright  ), spawn "mpc seek +10")
    , ((mod1Mask .|. shiftMask, xK_bracketright  ), spawn "mpc seek +60")
    , ((mod1Mask,               xK_Right  ), spawn "mpc vol +5")
    , ((mod1Mask,               xK_Left  ), spawn "mpc vol -5")
    , ((mod1Mask,               xK_apostrophe  ), spawn "mpc seek 0%")
    , ((mod1Mask .|. controlMask,  xK_space  ), spawn "mpc single")
    , ((mod1Mask .|. shiftMask, xK_s  ), spawn "maim -s ~/Data/screenshots/$(date +%Y-%m-%d-%s).png")
    , ((0 .|. shiftMask,               xK_Print  ), spawn "maim -s ~/Data/screenshots/$(date +%Y-%m-%d-%s).png")
    , ((mod1Mask,                xK_Print  ), spawn "maim ~/Data/screenshots/$(date +%Y-%m-%d-%s).png")
    , ((mod1Mask,                xK_s  ), spawn "maim ~/Data/screenshots/$(date +%Y-%m-%d-%s).png")
    , ((0, xF86XK_AudioMute  ), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
    , ((0, xF86XK_AudioRaiseVolume  ), spawn "pactl set-sink-volume @DEFAULT_SINK@ +3%")
    , ((0, xF86XK_AudioLowerVolume  ), spawn "pactl set-sink-volume @DEFAULT_SINK@ -3%")
    , ((0, xF86XK_AudioPlay  ), spawn "mpc toggle")
    , ((0, xF86XK_AudioPrev  ), spawn "mpc prev")
    , ((0, xF86XK_AudioNext  ), spawn "mpc next")
    , ((0, xF86XK_MonBrightnessUp  ), spawn "xbacklight -inc 5")
    , ((0, xF86XK_MonBrightnessDown  ), spawn "xbacklight -dec 5")
    , ((0, xF86XK_HomePage  ), spawn (myBrowser))

    , ((mod1Mask .|. controlMask, xK_v  ), spawn (myTerminal ++ " -e pulsemixer"))
    , ((modm,                     xK_n  ), spawn (myTerminal ++ " -e newsboat"))
    , ((modm,                     xK_m  ), spawn (myTerminal ++ " -e neomutt"))
    , ((modm,                     xK_a  ), spawn (myTerminal ++ " -e ranger"))
    , ((modm .|. shiftMask,       xK_e  ), spawn (myTerminal ++ " -e gotop"))
    , ((mod1Mask,                 xK_r  ), spawn "radio-listen")
    , ((mod1Mask,                 xK_Menu  ), spawn "radio-listen")
    , ((modm .|. shiftMask,       xK_BackSpace ), spawn "power")
    , ((mod1Mask .|. shiftMask, xK_w     ), spawn "set-wallpaper")
    , ((modm .|. shiftMask, xK_c     ), spawn "gimp")
    , ((modm,               xK_grave     ), spawn "alacritty")
    , ((modm .|. shiftMask, xK_grave     ), spawn "libreoffice")
    , ((modm .|. shiftMask, xK_d        ), spawn "rofi -show drun")
    , ((modm .|. shiftMask, xK_w        ), spawn "open-bookmarks")
    , ((modm,           xK_x     ), spawn "pcmanfm")
    , ((modm,           xK_slash ), spawn "web-search")
    , ((modm .|. shiftMask, xK_slash ), spawn "browser-launch")
    , ((modm,           xK_Menu  ), spawn "rofi -show emoji")
    , ((modm,           xK_d     ), spawn "clipgrab")
    , ((modm,           xK_e     ), spawn "emacsclient -c || emacs")
    , ((modm,           xK_c     ), spawn "galculator")
    , ((modm,           xK_v     ), spawn "qbittorrent")
    , ((modm,           xK_w     ), spawn (myBrowser))
    , ((modm .|. shiftMask, xK_v     ), spawn "minitube")
    , ((modm .|. controlMask, xK_x     ), spawn "slock")
    ]
    ++

    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    [((m .|. controlMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_1, xK_2, xK_3] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    ]

-- Layouts:
myLayout = smartBorders $ avoidStruts (tiled ||| Mirror tiled ||| Full)
  where
    tiled   = Tall nmaster delta ratio
    nmaster = 1
    ratio   = 1/2
    delta   = 3/100

-- Window rules:
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , (isFullscreen --> doFullFloat)
    , resource  =? "kdesktop"       --> doIgnore ]

fullscreenFix :: XConfig a -> XConfig a
fullscreenFix c = c {
                      startupHook = startupHook c +++ setSupportedWithFullscreen
                    }
                  where x +++ y = mappend x y

setSupportedWithFullscreen :: X ()
setSupportedWithFullscreen = withDisplay $ \dpy -> do
    r <- asks theRoot
    a <- getAtom "_NET_SUPPORTED"
    c <- getAtom "ATOM"
    supp <- mapM getAtom ["_NET_WM_STATE_HIDDEN"
                         ,"_NET_WM_STATE_FULLSCREEN"
                         ,"_NET_NUMBER_OF_DESKTOPS"
                         ,"_NET_CLIENT_LIST"
                         ,"_NET_CLIENT_LIST_STACKING"
                         ,"_NET_CURRENT_DESKTOP"
                         ,"_NET_DESKTOP_NAMES"
                         ,"_NET_ACTIVE_WINDOW"
                         ,"_NET_WM_DESKTOP"
                         ,"_NET_WM_STRUT"
                         ]
    io $ changeProperty32 dpy r a c propModeReplace (fmap fromIntegral supp)

 --   setWMName "xmonad"

-- Event handling
--myEventHook = mempty

-- Status bars and logging
-- myLogHook = return ()

-- Startup hook
-- myStartupHook = return ()

-- Now run xmonad with all the defaults we set up.

main = do
   xmproc <- spawnPipe "xmobar $HOME/.config/xmobar/config"
   xmonad $ docks $ fullscreenFix $ defaultConfig {
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

        keys               = myKeys,
        mouseBindings      = myMouseBindings,

        layoutHook         = myLayout,
        manageHook         = myManageHook <+> manageDocks, 
        handleEventHook    = docksEventHook <+> fullscreenEventHook,
        --logHook            =  myLogHook,
        logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "white" "" . shorten 100
                        }
        --startupHook        = myStartupHook
    }
