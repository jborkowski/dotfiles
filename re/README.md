# Restaumatic Dev Container Customizations

Personal dev container customization files for working with Restaumatic's product. These files extend the base Restaumatic dev container with custom tooling, networking, and environment configuration.

## Usage

**Important:** Before using these files, you must unlock the repository with git-crypt since `docker-compose.local.yaml` contains encrypted secrets (AWS credentials, environment variables, etc.):

```bash
cd /path/to/this/dotfiles/repo
git-crypt unlock
```

Then use GNU Stow to deploy these files to your Restaumatic dev container repository:

```bash
stow re -t /path/to/restaumatic-devcontainer-repo
```

## Files

### Core Configuration

- **`Dockerfile.local`** - Custom Dockerfile that extends the base Restaumatic dev container with:
  - Neovim built from source
  - tree-sitter library
  - Tailscale VPN client and daemon
  - Custom development tools (rust tools, fzf, etc.)
  - Personal dotfiles from GitHub
  - Bitwarden Secrets Manager integration
  - Automated git-crypt unlocking for encrypted dotfiles

- **`docker-compose.local.yaml`** - Docker Compose override file that:
  - **Encrypted with git-crypt** - Contains sensitive credentials and configuration
  - Configures Tailscale networking (requires NET_ADMIN capabilities and /dev/net/tun)
  - Sets AWS credentials for Terraform operations
  - Mounts personal persistent volumes (.ssh, .gnupg, .config, notes, etc.)
  - Loads secrets from `~/.config/zsh/.env.secret`
  - Exposes database (5433) and nginx (8888) ports to host

### Tailscale Scripts

Scripts for managing Tailscale VPN connectivity within the dev container:

- **`start_tailscale.sh`** - Starts the Tailscale daemon and auto-connects using TS_AUTHKEY if available
- **`connect_tailscale.sh`** - Simplified connection script with QR code authentication
- **`generate_ts_key.sh`** - Host-side script to generate ephemeral Tailscale auth keys using OAuth (requires TS_OAUTH_CLIENT_ID and TS_OAUTH_CLIENT_SECRET)

### Secret Management

- **`load_bws_secrets.sh`** - Loads secrets from Bitwarden Secrets Manager when BW_SECRET environment variable is set
- **`import_gpg_keys.sh`** - Imports GPG keys from encrypted backup archive

## Environment Variables

The following environment variables can be configured:

### Tailscale
- `TS_AUTHKEY` - Ephemeral auth key for automatic Tailscale connection
- `TS_HOSTNAME` - Custom hostname for the Tailscale node (default: "devcontainer")
- `TS_OAUTH_CLIENT_ID` - OAuth client ID for generating auth keys
- `TS_OAUTH_CLIENT_SECRET` - OAuth client secret for generating auth keys

### Bitwarden
- `BW_SECRET` - Bitwarden Secrets Manager access token

### AWS (for Terraform)
- `AWS_ACCESS_KEY_ID`
- `AWS_SECRET_ACCESS_KEY`
- `AWS_DEFAULT_REGION`

## Features

### Networking
- Tailscale VPN integration with subnet routing (10.244.129.0/24)
- Automatic connection on container start
- Support for both auth key and interactive authentication

### Development Tools
- Latest Neovim with tree-sitter support
- Rust toolchain with common CLI tools (eza, ripgrep, fd, bottom, gitu, xcp)
- Modern fzf fuzzy finder
- Node.js/TypeScript/PureScript tooling
- Python tools via uv
- Claude Code CLI

### Secrets Management
- Bitwarden Secrets Manager integration
- GPG key management
- Encrypted dotfiles support via git-crypt

### Persistent Storage
Persistent Docker volumes for:
- SSH keys
- Emacs configuration
- Vim/Neovim configuration
- Personal notes
- Claude Code configuration
- GPG keyring
- Tailscale state

## Notes

- These files are designed to work with Restaumatic's base dev container image
- The Dockerfile uses multi-stage builds to optimize layer caching
- Tailscale requires NET_ADMIN and NET_RAW capabilities
- AWS credentials are configured for the specific Restaumatic infrastructure account
- Personal notes are bind-mounted from `/data/notes` on the host
