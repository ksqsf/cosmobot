{-|
Module      : Bot.Plugin.Sandbox
Description : Bubblewrap launch arguments for external plugins
Stability   : experimental
-}

module Bot.Plugin.Sandbox
  ( bubblewrapExecutable
  , bubblewrapArguments
  , translateSandboxMediaPath
  )
where

import Bot.Prelude
import System.FilePath

bubblewrapExecutable :: FilePath
bubblewrapExecutable = "/usr/bin/bwrap"

-- | Run one bundle with no host filesystem view beyond its writable bundle,
-- the read-only media cache, and the runtime files needed by ordinary dynamic
-- executables. Network isolation is deliberately undone for plugin host calls.
bubblewrapArguments :: FilePath -> FilePath -> Text -> [String]
bubblewrapArguments bundleDirectory mediaDirectory pluginId =
  [ "--unshare-all"
  , "--unshare-user"
  , "--share-net"
  , "--die-with-parent"
  , "--new-session"
  , "--disable-userns"
  , "--cap-drop", "ALL"
  , "--clearenv"
  , "--setenv", "PATH", "/usr/bin:/bin"
  , "--setenv", "HOME", "/plugin"
  , "--setenv", "COSMOBOT_PLUGIN_CONFIG", "/plugin/config.toml"
  , "--proc", "/proc"
  , "--dev", "/dev"
  , "--dir", "/usr"
  , "--dir", "/usr/bin"
  , "--ro-bind-try", "/usr/bin/env", "/usr/bin/env"
  , "--ro-bind-try", "/usr/bin/python3", "/usr/bin/python3"
  , "--ro-bind", "/usr/lib", "/usr/lib"
  , "--ro-bind-try", "/usr/lib64", "/usr/lib64"
  , "--symlink", "usr/bin", "/bin"
  , "--symlink", "usr/lib", "/lib"
  , "--symlink", "usr/lib64", "/lib64"
  , "--ro-bind-try", "/etc/resolv.conf", "/etc/resolv.conf"
  , "--ro-bind-try", "/etc/hosts", "/etc/hosts"
  , "--ro-bind-try", "/etc/nsswitch.conf", "/etc/nsswitch.conf"
  , "--ro-bind-try", "/etc/ssl/certs", "/etc/ssl/certs"
  , "--ro-bind-try", "/etc/ld.so.cache", "/etc/ld.so.cache"
  , "--bind", bundleDirectory, "/plugin"
  , "--ro-bind", bundleDirectory </> "config.toml", "/plugin/config.toml"
  , "--ro-bind", mediaDirectory, "/media"
  , "--tmpfs", "/tmp"
  , "--chdir", "/plugin"
  , "--"
  , "/plugin/" <> toString pluginId
  ]

-- | Translate only paths actually below the mounted media directory.
translateSandboxMediaPath :: FilePath -> FilePath -> Maybe FilePath
translateSandboxMediaPath mediaDirectory path = do
  let relative = makeRelative (normalise mediaDirectory) (normalise path)
  guard (isRelative relative && relative /= ".." && not (".." `elem` splitDirectories relative))
  pure ("/media" </> relative)
