{-# LANGUAGE OverloadedStrings #-}

-- vim: set et ts=2 :

module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Text
import qualified GI.Gtk as Gtk
import qualified GI.Gtk.Objects.Overlay as Gtk
import System.Log.Logger
import System.Taffybar
import System.Taffybar.Context (TaffybarConfig(..))
import System.Taffybar.Hooks
import System.Taffybar.Information.CPU
import System.Taffybar.Information.Memory
import System.Taffybar.SimpleConfig
import System.Taffybar.Widget
import System.Taffybar.Widget.Generic.PollingGraph

transparent, yellow1, yellow2, green1, green2, taffyBlue
  :: (Double, Double, Double, Double)
transparent = (0.0, 0.0, 0.0, 0.0)
yellow1 = (0.9453125, 0.93671875, 0.6109375, 1.0)
yellow2 = (0.9921875, 0.996875, 0.62421875, 1.0)
green1 = (0.5, 1, 0.5, 1)
green2 = (1, 0.5, 1, 0.5)
taffyBlue = (0.629, 0.988, 0.953, 1)

myGraphConfig, netCfg, memCfg, cpuCfg :: GraphConfig
myGraphConfig =
  defaultGraphConfig
  { graphPadding = 0
  , graphBorderWidth = 0
  , graphWidth = 75
  , graphBackgroundColor = transparent
  , graphDataStyles = repeat Line
  }

netCfg = myGraphConfig
  { graphDataColors = [yellow1, yellow2]
  , graphLabel = Just "net"
  , graphDataStyles = repeat Line
  }

memCfg = myGraphConfig
  { graphDataColors = [taffyBlue]
  , graphLabel = Just "mem"
  , graphDataStyles = repeat Line
  }

cpuCfg = myGraphConfig
  { graphDataColors = [green1, green2]
  , graphLabel = Just "cpu"
  }

memCallback :: IO [Double]
memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

cpuCallback :: IO [Double]
cpuCallback = do
  (_, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

withClass :: MonadIO m => Data.Text.Text -> m Gtk.Widget -> m Gtk.Widget
withClass klass builder = builder >>= flip widgetSetClassGI klass

myConfig :: TaffybarConfig
myConfig =
  let myWorkspacesConfig =
        defaultWorkspacesConfig
        { minIcons = 1
        , widgetGap = 0
        , showWorkspaceFn = hideEmpty
        }
      workspaces = workspacesNew myWorkspacesConfig
      cpu = withClass "graph" $
        pollingGraphNew cpuCfg 5 cpuCallback
      mem = withClass "graph" $
        pollingGraphNew memCfg 5 memCallback
      net = withClass "graph" $
        networkGraphNew netCfg Nothing
      clock = withClass "clock" $
        textClockNewWith ClockConfig
          { clockTimeZone = Nothing
            , clockTimeLocale = Nothing
            , clockFormatString = "%a %b %H:%M"
            , clockUpdateStrategy = RoundedTargetInterval 6 0.0
          }
      analogClock = withClass "analog-clock" $
        textClockNewWith ClockConfig
          { clockTimeZone = Nothing
            , clockTimeLocale = Nothing
            , clockFormatString = "T%H:%M"
            , clockUpdateStrategy = RoundedTargetInterval 6 0.0
          }

      layout = layoutNew defaultLayoutConfig
      -- See https://github.com/taffybar/gtk-sni-tray#statusnotifierwatcher
      -- for a better way to set up the sni tray
      tray = sniTrayNew
      myConfig = defaultSimpleTaffyConfig
        { startWidgets =
            workspaces : map (>>= buildContentsBox) [ layout ]
        , endWidgets = map (>>= buildContentsBox)
          [ batteryIconNew
          , analogClock
          , clock
          , tray
          , cpu
          , mem
          , net
          , mpris2New
          ]
        , barPosition = Top
        , barPadding = 3
        , barHeight = 24
        , widgetSpacing = 0
        }
  in withBatteryRefresh $ withLogServer $
     withToggleServer $ toTaffyConfig myConfig

main = do
    startTaffybar myConfig
