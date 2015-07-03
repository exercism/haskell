# -*- mode: ruby -*-
# vi: set ft=ruby :

# You only need to use this if you're trying to emulate Travis

VAGRANTFILE_API_VERSION = "2"

$script = <<SCRIPT
add-apt-repository -y ppa:hvr/ghc
apt-get update
apt-get install -q -y build-essential git libgmp3c2 pigz zlib1g-dev \
  ghc-7.6.3 ghc-7.8.4 ghc-7.10.1 cabal-install-1.22
apt-get clean
SCRIPT

Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|
  # Travis uses Ubuntu 12.04 LTS
  config.vm.box = "ubuntu/precise64"
  config.vm.provision "shell", inline: $script
  config.vm.provider "virtualbox" do |v|
    v.memory = 1024
    v.cpus = 2
  end
end
