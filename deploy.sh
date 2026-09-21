#!/bin/bash
# ============================================================
# Deploy WPP711 Shiny App to Oracle Cloud Free Tier
# Run this script on your Oracle Cloud VM (Ubuntu/Debian)
# ============================================================

set -e

echo "=== WPP711 Deployment Script ==="
echo ""

# 1. Update system
echo "[1/6] Updating system..."
sudo apt-get update && sudo apt-get upgrade -y

# 2. Install Docker
echo "[2/6] Installing Docker..."
if ! command -v docker &> /dev/null; then
    curl -fsSL https://get.docker.com | sudo bash
    sudo usermod -aG docker $USER
    echo "Docker installed. You may need to logout/login for group changes."
else
    echo "Docker already installed."
fi

# 3. Install Docker Compose
echo "[3/6] Installing Docker Compose..."
if ! command -v docker-compose &> /dev/null; then
    sudo curl -L "https://github.com/docker/compose/releases/latest/download/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
    sudo chmod +x /usr/local/bin/docker-compose
else
    echo "Docker Compose already installed."
fi

# 4. Clone repository
echo "[4/6] Cloning repository..."
if [ ! -d "/opt/wpp711" ]; then
    sudo git clone https://github.com/el-kizmi/wpp711.git /opt/wpp711
else
    cd /opt/wpp711 && sudo git pull
fi

# 5. Build and run
echo "[5/6] Building Docker image..."
cd /opt/wpp711
sudo docker-compose up -d --build

# 6. Open firewall
echo "[6/6] Configuring firewall..."
if command -v ufw &> /dev/null; then
    sudo ufw allow 3838/tcp
    sudo ufw reload
fi

echo ""
echo "=== Deployment Complete ==="
echo "Aplikasi berjalan di: http://$(curl -s ifconfig.me):3838"
echo ""
echo "Perintah berguna:"
echo "  Lihat log:        sudo docker-compose logs -f"
echo "  Stop:             sudo docker-compose down"
echo "  Restart:          sudo docker-compose restart"
echo "  Update:           cd /opt/wpp711 && sudo git pull && sudo docker-compose up -d --build"
