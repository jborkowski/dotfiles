#!/bin/bash
# Install script for bash enhancements and plugins

set -e

BASH_CONFIG_DIR="${XDG_CONFIG_HOME:-$HOME/.config}/bash"
PLUGINS_DIR="$BASH_CONFIG_DIR/plugins"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${GREEN}Installing bash enhancements and plugins...${NC}"

# Create plugins directory if it doesn't exist
mkdir -p "$PLUGINS_DIR"

# Function to clone or update a git repository
install_or_update() {
    local repo_url="$1"
    local target_dir="$2"
    local repo_name=$(basename "$target_dir")

    if [ -d "$target_dir" ]; then
        echo -e "${YELLOW}Updating $repo_name...${NC}"
        cd "$target_dir"
        git pull --quiet
        cd - > /dev/null
    else
        echo -e "${GREEN}Installing $repo_name...${NC}"
        git clone --depth=1 "$repo_url" "$target_dir"
    fi
}

# Install Starship prompt (similar to Powerlevel10k for zsh)
if ! command -v starship &> /dev/null; then
    echo -e "${GREEN}Installing Starship prompt...${NC}"
    if command -v curl &> /dev/null; then
        curl -sS https://starship.rs/install.sh | sh
    else
        echo -e "${YELLOW}Please install Starship manually from https://starship.rs${NC}"
    fi
else
    echo -e "${YELLOW}Starship already installed${NC}"
fi

# Install bash-preexec (for better command hooks)
install_or_update "https://github.com/rcaloras/bash-preexec.git" "$PLUGINS_DIR/bash-preexec"

# Install bash-completion
if [[ "$OSTYPE" == "darwin"* ]]; then
    if command -v brew &> /dev/null; then
        if ! brew list bash-completion &> /dev/null; then
            echo -e "${GREEN}Installing bash-completion via Homebrew...${NC}"
            brew install bash-completion
        else
            echo -e "${YELLOW}bash-completion already installed${NC}"
        fi
    fi
elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
    if command -v apt-get &> /dev/null; then
        echo -e "${GREEN}Installing bash-completion via apt...${NC}"
        sudo apt-get update && sudo apt-get install -y bash-completion
    elif command -v yum &> /dev/null; then
        echo -e "${GREEN}Installing bash-completion via yum...${NC}"
        sudo yum install -y bash-completion
    elif command -v pacman &> /dev/null; then
        echo -e "${GREEN}Installing bash-completion via pacman...${NC}"
        sudo pacman -S --noconfirm bash-completion
    fi
fi

# Install ble.sh (bash line editor - provides autosuggestions and syntax highlighting)
if [ ! -d "$PLUGINS_DIR/blesh" ]; then
    echo -e "${GREEN}Installing ble.sh for autosuggestions and syntax highlighting...${NC}"
    git clone --recursive --depth 1 https://github.com/akinomyoga/ble.sh.git "$PLUGINS_DIR/blesh"
    cd "$PLUGINS_DIR/blesh"
    make install PREFIX="$HOME/.local"
    cd - > /dev/null
else
    echo -e "${YELLOW}ble.sh already installed${NC}"
    echo -e "${YELLOW}To update, run: cd $PLUGINS_DIR/blesh && git pull && make install PREFIX=$HOME/.local${NC}"
fi

# Install fzf (fuzzy finder)
if ! command -v fzf &> /dev/null; then
    echo -e "${GREEN}Installing fzf...${NC}"
    git clone --depth 1 https://github.com/junegunn/fzf.git "$HOME/.fzf"
    "$HOME/.fzf/install" --all --no-zsh --no-fish
else
    echo -e "${YELLOW}fzf already installed${NC}"
fi

# Install bash-git-prompt (enhanced git prompt)
install_or_update "https://github.com/magicmonty/bash-git-prompt.git" "$PLUGINS_DIR/bash-git-prompt"

# Install sensible bash (better defaults)
install_or_update "https://github.com/mrzool/bash-sensible.git" "$PLUGINS_DIR/bash-sensible"

# Create starship config if it doesn't exist
if [ ! -f "$HOME/.config/starship.toml" ]; then
    echo -e "${GREEN}Creating default Starship configuration...${NC}"
    mkdir -p "$HOME/.config"
    cat > "$HOME/.config/starship.toml" << 'EOF'
# Starship configuration

# Format of the prompt
format = """
$username\
$hostname\
$directory\
$git_branch\
$git_status\
$git_state\
$cmd_duration\
$line_break\
$python\
$rust\
$nodejs\
$character"""

# Configure directory display
[directory]
truncation_length = 3
truncate_to_repo = false

# Configure git branch display
[git_branch]
symbol = " "
format = "[$symbol$branch]($style) "

# Configure git status display
[git_status]
format = '([\[$all_status$ahead_behind\]]($style) )'
conflicted = "⚔️ "
ahead = "⬆️ ×${count}"
behind = "⬇️ ×${count}"
diverged = "🔀 "
untracked = "📝×${count}"
stashed = "📦 "
modified = "📌×${count}"
staged = "✅×${count}"
renamed = "✏️ ×${count}"
deleted = "🗑️ ×${count}"

# Configure character (prompt symbol)
[character]
success_symbol = "[➜](bold green)"
error_symbol = "[✗](bold red)"

# Configure command duration
[cmd_duration]
min_time = 500
format = "took [$duration](bold yellow)"

# Language/Framework specific
[python]
symbol = "🐍 "
format = "[$symbol$version]($style) "

[rust]
symbol = "🦀 "
format = "[$symbol$version]($style) "

[nodejs]
symbol = "⬢ "
format = "[$symbol$version]($style) "
EOF
fi

echo -e "${GREEN}✓ Bash plugins installation complete!${NC}"
echo -e "${YELLOW}Note: You may need to restart your terminal or source your .bashrc for changes to take effect.${NC}"
echo -e "${YELLOW}Run: source ~/.bashrc${NC}"
