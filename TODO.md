# FIXME
 - hardwired path in `batch_analyze.sh` (see `directory_path` var)
 - black/whitelisted modules?
 - fix some predicate names
 - better interface for `find.pl` module, e.g.:
   ```
   % Similar to debugger?
   find_module(Mod)
   nofind_module(Mod)
   find_bundle(Bundle)
   nofind_bundle(Bundle)
   find_dir(Dir)
   nofind_dir(Dir)
   nofind/0 % removes all selection
   
   % Regenerate dumps (e.g. call batch_analyze.sh, ciaopp_batch, etc.)
   find_update/0 % que llame al script para regenerar los dumps, por ejemplo
   
   find_pred(...)
   
   set_find_flag(Flag, Value)
   ```

