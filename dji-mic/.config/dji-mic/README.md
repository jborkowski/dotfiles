# dji-mic

Pull DJI Wireless Mic recordings into a dated VoiceNotes folder, and
auto-pull when the transmitter is mounted.

This is the reusable copy. The one-off VoiceNotes script is separate and
must not be edited while a sync is running.

```
dji-mic detect
dji-mic pull --dry-run
dji-mic pull              # copy, verify, delete, then eject
dji-mic pull --no-eject   # same, but leave the volume mounted
dji-mic pull --keep       # copy only (still ejects unless --no-eject)
dji-mic eject             # diskutil eject, safe to unplug
dji-mic watch enable      # launchd WatchPaths on /Volumes
dji-mic watch status
dji-mic watch disable
```

Device match (any hit is enough):

- media name `WIRELESS MIC TX2`
- volume UUID `E0A935FA-012A-35B8-91E9-799C8510DE42`
- USB vendor/product `4310:45067` plus a `DJI_Audio_*` folder

USB serial `0123456798AB` is generic and is not used.

Remember: do not eject on every plug-in. Eject is the default only after
a successful copy. Empty remounts stay mounted (`--no-eject` / `EJECT_AFTER=0`).

Edit `~/.config/dji-mic/config` to point at another destination or device.
Logs go to `~/Library/Logs/dji-mic.log`.
