#!/bin/bash
# Setup script for bash configuration

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Get the directory where this script is located
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}     Bash Configuration Setup Script    ${NC}"
echo -e "${BLUE}========================================${NC}"
echo

# Function to backup existing files
backup_file() {
    local file="$1"
    if [ -f "$file" ]; then
        local backup="${file}.backup.$(date +%Y%m%d_%H%M%S)"
        echo -e "${YELLOW}Backing up existing $(basename "$file") to $(basename "$backup")${NC}"
        mv "$file" "$backup"
    fi
}

# Function to create symlink
create_symlink() {
    local source="$1"
    local target="$2"

    if [ -L "$target" ]; then
        echo -e "${YELLOW}Removing existing symlink: $target${NC}"
        rm "$target"
    elif [ -f "$target" ]; then
        backup_file "$target"
    elif [ -d "$target" ]; then
        echo -e "${RED}Error: $target is a directory. Please remove it manually.${NC}"
        return 1
    fi

    echo -e "${GREEN}Creating symlink: $target -> $source${NC}"
    ln -sf "$source" "$target"
}

# Check if running from the correct directory
if [ ! -f "$SCRIPT_DIR/.bashrc" ] || [ ! -f "$SCRIPT_DIR/.bash_profile" ]; then
    echo -e "${RED}Error: This script must be run from the bash configuration directory.${NC}"
    echo -e "${RED}Please cd into the directory containing the bash config files.${NC}"
    exit 1
fi

echo -e "${GREEN}Step 1: Creating necessary directories...${NC}"
mkdir -p "$HOME/.config/bash"
mkdir -p "$HOME/.local/bin"
mkdir -p "$HOME/.local/share"

echo -e "${GREEN}Step 2: Setting up configuration files...${NC}"

# Copy or symlink the .config/bash directory
if [ -d "$SCRIPT_DIR/.config/bash" ]; then
    # Check if target exists and handle appropriately
    if [ -d "$HOME/.config/bash" ]; then
        echo -e "${YELLOW}Directory ~/.config/bash already exists.${NC}"
        read -p "Do you want to (b)ackup and replace, (m)erge, or (s)kip? [b/m/s]: " choice
        case "$choice" in
            b|B)
                backup_dir="$HOME/.config/bash.backup.$(date +%Y%m%d_%H%M%S)"
                echo -e "${YELLOW}Backing up to $backup_dir${NC}"
                mv "$HOME/.config/bash" "$backup_dir"
                cp -r "$SCRIPT_DIR/.config/bash" "$HOME/.config/"
                ;;
            m|M)
                echo -e "${GREEN}Merging configurations...${NC}"
                cp -rn "$SCRIPT_DIR/.config/bash/." "$HOME/.config/bash/"
                ;;
            s|S)
                echo -e "${YELLOW}Skipping .config/bash setup${NC}"
                ;;
            *)
                echo -e "${RED}Invalid choice. Exiting.${NC}"
                exit 1
                ;;
        esac
    else
        echo -e "${GREEN}Copying .config/bash directory...${NC}"
        cp -r "$SCRIPT_DIR/.config/bash" "$HOME/.config/"
    fi
fi

# Create symlinks for root dotfiles
echo -e "${GREEN}Step 3: Creating symlinks for root configuration files...${NC}"
create_symlink "$SCRIPT_DIR/.bashrc" "$HOME/.bashrc"
create_symlink "$SCRIPT_DIR/.bash_profile" "$HOME/.bash_profile"

# Check for existing zsh config
if [ -f "$HOME/.zshrc" ] || [ -f "$HOME/.zshenv" ]; then
    echo
    echo -e "${YELLOW}Note: Existing zsh configuration detected.${NC}"
    echo -e "${YELLOW}The bash configuration has been set up alongside it.${NC}"
    echo -e "${YELLOW}To use bash as your default shell, run: chsh -s $(which bash)${NC}"
fi

# Install plugins
echo
echo -e "${GREEN}Step 4: Installing plugins and enhancements...${NC}"
read -p "Do you want to install bash plugins and enhancements? [y/N]: " install_plugins
if [[ "$install_plugins" =~ ^[Yy]$ ]]; then
    if [ -f "$HOME/.config/bash/install-plugins.sh" ]; then
        chmod +x "$HOME/.config/bash/install-plugins.sh"
        "$HOME/.config/bash/install-plugins.sh"
    else
        echo -e "${YELLOW}Plugin installation script not found. Skipping...${NC}"
    fi
else
    echo -e "${YELLOW}Skipping plugin installation.${NC}"
    echo -e "${YELLOW}You can run ~/.config/bash/install-plugins.sh later to install them.${NC}"
fi

# Check for required tools
echo
echo -e "${GREEN}Step 5: Checking for recommended tools...${NC}"

check_command() {
    local cmd="$1"
    local install_hint="$2"

    if command -v "$cmd" &> /dev/null; then
        echo -e "${GREEN}✓${NC} $cmd is installed"
    else
        echo -e "${YELLOW}✗${NC} $cmd is not installed. $install_hint"
    fi
}

check_command "nvim" "Install from https://neovim.io/"
check_command "fzf" "Will be installed by plugin script"
check_command "bat" "Install with: brew install bat (macOS) or apt install bat (Ubuntu)"
check_command "fd" "Install with: brew install fd (macOS) or apt install fd-find (Ubuntu)"
check_command "direnv" "Install from https://direnv.net/"
check_command "starship" "Will be installed by plugin script"

# Operating system specific checks
if [[ "$OSTYPE" == "darwin"* ]]; then
    check_command "brew" "Install from https://brew.sh/"
    if command -v brew &> /dev/null; then
        echo -e "${YELLOW}Tip: You can install missing tools with: brew install nvim bat fd direnv${NC}"
    fi
fi

# Create example .env.secret file if it doesn't exist
if [ ! -f "$HOME/.config/bash/.env.secret" ]; then
    echo
    echo -e "${GREEN}Step 6: Creating example .env.secret file...${NC}"
    cat > "$HOME/.config/bash/.env.secret" << 'EOF'
# Secret environment variables
# Add your API keys, tokens, and other sensitive data here
# This file is git-ignored and won't be committed

# Example:
# export GITHUB_TOKEN="your-token-here"
# export API_KEY="your-api-key-here"
EOF
    chmod 600 "$HOME/.config/bash/.env.secret"
    echo -e "${GREEN}Created ~/.config/bash/.env.secret for your private environment variables${NC}"
else
    echo -e "${YELLOW}~/.config/bash/.env.secret already exists, skipping...${NC}"
fi

echo
echo -e "${GREEN}========================================${NC}"
echo -e "${GREEN}     Setup Complete! 🎉                ${NC}"
echo -e "${GREEN}========================================${NC}"
echo
echo -e "${BLUE}Next steps:${NC}"
echo -e "1. Restart your terminal or run: ${GREEN}source ~/.bashrc${NC}"
echo -e "2. If you want bash as your default shell: ${GREEN}chsh -s $(which bash)${NC}"
echo -e "3. Customize your configuration in ${GREEN}~/.config/bash/.bashrc${NC}"
echo -e "4. Add secret environment variables to ${GREEN}~/.config/bash/.env.secret${NC}"
echo
echo -e "${YELLOW}Note: Some features require additional tools to be installed.${NC}"
echo -e "${YELLOW}Run the plugin installation script if you haven't already:${NC}"
echo -e "${GREEN}~/.config/bash/install-plugins.sh${NC}"
echo
echo -e "${BLUE}Enjoy your new bash environment! 🚀${NC}"
