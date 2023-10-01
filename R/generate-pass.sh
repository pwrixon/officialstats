#!/usr/bin/zsh

# Generate a secure password
password=$(openssl rand -base64 20 | tr -dc 'a-zA-Z0-9' | fold -w 16 | head -n 1)

# Prompt the user for login credentials
read -p "Enter your username: " username
read -sp "Enter your password: " user_password

# Save login credentials to iCloud Keychain
security add-generic-password -a "$username" -w "$user_password" -s "Login" -l "$(whoami) Login" -T /usr/bin/security

# Save the generated password to iCloud Keychain
security add-generic-password -a "Generated Password" -w "$password" -s "Generated Password" -l "$(whoami) Generated Password" -T /usr/bin/security

echo "Password and login credentials saved to iCloud Keychain."
