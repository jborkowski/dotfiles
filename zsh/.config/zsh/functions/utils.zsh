hl_howto() {
   local sources_dir="${AI_THAT_WORKS_SOURCES:-$HOME/sources}"
   local repo_dir="$sources_dir/ai-that-works"

   if [[ ! -d "$repo_dir" ]]; then
     mkdir -p "$sources_dir"
     gh repo clone ai-that-works/ai-that-works "$repo_dir"
   fi

   ln -sf "$repo_dir/HOWTO.md" ./HOWTO.md
   echo "→ $repo_dir/HOWTO.md  ->  ./HOWTO.md"
 }

